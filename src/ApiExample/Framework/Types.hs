module ApiExample.Framework.Types where

import Control.Monad.Reader (ReaderT)
import Data.Aeson (Key, ToJSON, Value)
import Data.Text
import Data.Time.Clock.POSIX (POSIXTime)
import Data.ULID (ULID)
import Data.Vault.Lazy qualified as Vault
import GHC.Generics (Generic)
import Hasql.Session qualified as HSession
import Hasql.Transaction qualified as Tx
import Network.Wai (Request)
import Servant (AuthProtect, Handler)
import Servant.Server (HasServer (ServerT))
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData)

type HandlerM = ReaderT AppCtx Handler

type RunDBIO = forall a. HSession.Session a -> HandlerM a

type AppTx = forall a. Tx.Transaction a -> HandlerM a

data AppCtx = AppCtx
  { runDBIO :: RunDBIO
  , tx :: AppTx
  , reqScopeCtx :: Vault.Vault -> ReqScopeCtx
  }

type ServerM api = ServerT api HandlerM

type Cookies = [(Text, Text)]

data Session = Session {userName :: Text, email :: Text}

type CookieAuth = AuthProtect "cookie"

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

type Logger = forall a. (Show a, ToJSON a) => Maybe [(Key, Value)] -> a -> IO ()

data LogLevel = Danger | Warning | Info deriving (Generic)
instance Show LogLevel where
  show Danger = "danger"
  show Warning = "warning"
  show Info = "info"

instance ToJSON LogLevel
