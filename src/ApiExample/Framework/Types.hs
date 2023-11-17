{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.Framework.Types where

import Control.Lens ((%~), (.~), (?~))
import Control.Lens.Lens ((&))
import Control.Monad.Reader (ReaderT)
import Data.Aeson (Key, ToJSON (..), Value)
import Data.OpenApi (OpenApiType (OpenApiString), ParamLocation (..), Referenced (..), description, in_, name, schema, type_)
import Data.OpenApi.Operation
import Data.Text
import Data.Time.Clock.POSIX (POSIXTime)
import Data.ULID (ULID)
import Data.Vault.Lazy qualified as Vault
import GHC.Generics (Generic)
import Hasql.Session qualified as HSession
import Hasql.Transaction qualified as Tx
import Network.Wai (Request)
import Servant (AuthProtect, Handler, Proxy (..), (:>))
import Servant.OpenApi.Internal
import Servant.Server (HasServer (ServerT))
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData)

type HandlerM = ReaderT AppCtx Handler

type RunDBIO = forall a. HSession.Session a -> HandlerM a

type AppTx = forall a. Tx.Transaction a -> HandlerM a

data AppCtx = AppCtx
  { _runDBIO :: RunDBIO
  , _tx :: AppTx
  , _reqScopeCtx :: Vault.Vault -> ReqScopeCtx
  }

type ServerM api = ServerT api HandlerM

type Cookies = [(Text, Text)]

data Session = Session {userName :: Text, email :: Text}

type CookieAuth = AuthProtect "cookie"

instance (HasOpenApi a) => HasOpenApi (CookieAuth :> a) where
  toOpenApi _ = toOpenApi (Proxy @a) & addDefaultResponse401 "session-id" & addParam param
   where
    param =
      mempty
        & name .~ "session-id"
        & description ?~ "session-id"
        & in_ .~ ParamCookie
        & schema ?~ Inline (mempty & type_ ?~ OpenApiString)
    addDefaultResponse401 pname = setResponseWith (\old _new -> alter401 old) 401 (return response401)
     where
      sname = markdownCode pname
      alter401 = description %~ (<> (" or " <> sname))
      response401 = mempty & description .~ "description401"

type instance AuthServerData CookieAuth = Session

type AppAuthHandler = AuthHandler Request Session

data ReqScopeCtx = ReqScopeCtx
  { accessId :: ULID
  , reqAt :: POSIXTime
  , loggers :: Loggers
  }

data Loggers = Loggers
  { danger :: Logger
  , warn :: Logger
  , info :: Logger
  }

type Logger = Maybe [(Key, Value)] -> forall a. (Show a, ToJSON a) => a -> IO ()

data LogLevel = Danger | Warning | Info deriving (Generic)

instance ToJSON LogLevel where
  toJSON Danger = toJSON @Text "danger"
  toJSON Warning = toJSON @Text "warning"
  toJSON Info = toJSON @Text "info"
