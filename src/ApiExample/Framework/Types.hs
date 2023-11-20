{-# LANGUAGE BlockArguments #-}

module ApiExample.Framework.Types where

import ApiExample.Framework.Alias ((:>>))
import Data.Aeson (Key, ToJSON (..), Value)
import Data.Text
import Data.Time.Clock.POSIX (POSIXTime)
import Data.ULID (ULID)
import Data.Vault.Lazy qualified as Vault
import Effectful (Eff, Effect, IOE, liftIO, runEff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Dynamic (Error, runError)
import Effectful.Error.Dynamic qualified as Effectful
import Effectful.Reader.Dynamic
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import Hasql.Session qualified as HSession
import Hasql.Transaction qualified as Tx
import Network.Wai (Request)
import Servant hiding ((:>))
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData)

data WrappedHandler :: Effect where
  WrapHandler :: (Handler a) -> WrappedHandler m a

makeEffect ''WrappedHandler

runWrappedHandler :: (IOE :> es, Error ServerError :> es) => Eff (WrappedHandler : es) a -> Eff es a
runWrappedHandler = interpret handler
 where
  handler _ (WrapHandler h) = liftIO (runHandler h) >>= either Effectful.throwError pure

runHandlerM :: AppCtx -> HandlerM a -> Handler a
runHandlerM ctx e = liftIO (run e) >>= either (const $ throwError err500) pure
 where
  run = runEff . runError @ServerError . runWrappedHandler . runReader ctx

runReaderReqScopeCtx :: HandlerWithReqScopeCtx a -> Vault.Vault -> HandlerM a
runReaderReqScopeCtx h v = do
  AppCtx{_reqScopeCtx} <- ask
  runReader (_reqScopeCtx v) h

runReaderReqScopeCtx' :: Vault -> HandlerWithReqScopeCtx a -> HandlerM a
runReaderReqScopeCtx' = flip runReaderReqScopeCtx

type ServerM api = ServerT api HandlerM

type BaseEffectStack = '[Reader AppCtx, WrappedHandler, Error ServerError, IOE]

type HandlerM = Eff BaseEffectStack

type HandlerWithReqScopeCtx = Eff (Reader ReqScopeCtx : BaseEffectStack)

type WithVault method x y = Vault :>> method x y

type RunDBIO = forall a. HSession.Session a -> HandlerWithReqScopeCtx a

type AppTx = forall a. Tx.Transaction a -> HandlerWithReqScopeCtx a

data AppCtx = AppCtx
  { _runDBIO :: RunDBIO
  , _tx :: AppTx
  , _reqScopeCtx :: Vault.Vault -> ReqScopeCtx
  }

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

type Logger = Maybe [(Key, Value)] -> forall a. (Show a, ToJSON a) => a -> IO ()

data LogLevel = Danger | Warning | Info deriving (Generic)

instance ToJSON LogLevel where
  toJSON Danger = toJSON @Text "danger"
  toJSON Warning = toJSON @Text "warning"
  toJSON Info = toJSON @Text "info"
