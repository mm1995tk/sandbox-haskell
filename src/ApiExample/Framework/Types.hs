{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.Framework.Types where

import Data.Aeson (Key, ToJSON (..), Value)
import Data.Text
import Data.Time.Clock.POSIX (POSIXTime)
import Data.ULID (ULID)
import Data.Vault.Lazy qualified as Vault
import Effectful (Eff, Effect, IOE)
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import Hasql.Pool (UsageError)
import Hasql.Session qualified as HSession
import Hasql.Transaction qualified as Tx
import Servant (
  Handler,
  HasServer (ServerT),
  ServerError,
 )

data WrappedHandler :: Effect where
  WrapHandler :: (Handler a) -> WrappedHandler m a

makeEffect ''WrappedHandler

data RaiseTransaction :: Effect where
  RaiseTransaction :: (Tx.Transaction a) -> RaiseTransaction m a

makeEffect ''RaiseTransaction

type ServerM api = ServerT api HandlerM

type HandlerM = Eff BaseEffectStack

type HandlerWithReqScopeCtx = Eff (Reader ReqScopeCtx : BaseEffectStack)

type BaseEffectStack = '[Reader AppCtx, WrappedHandler, Error ServerError, IOE]

newtype AppCtx = AppCtx (Vault.Vault -> ReqScopeCtx)

data ReqScopeCtx = ReqScopeCtx
  { _runDBIO :: RunDBIO
  , _tx :: AppTx
  , runDBIO' :: forall a. HSession.Session a -> IO (Either UsageError a)
  , accessId :: ULID
  , reqAt :: POSIXTime
  , loggers :: Loggers
  }

type RunDBIO = forall a. HSession.Session a -> HandlerWithReqScopeCtx a

type AppTx = forall a. Tx.Transaction a -> HandlerWithReqScopeCtx a

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
