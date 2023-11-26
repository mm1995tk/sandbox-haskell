{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.Framework.Types where

import Data.Aeson (Key, ToJSON (..), Value)
import Data.Text
import Data.Time.Clock.POSIX (POSIXTime)
import Data.ULID (ULID)
import Effectful (Eff, IOE)
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic
import GHC.Generics (Generic)
import Hasql.Pool (UsageError)
import Hasql.Session qualified as HSession
import Hasql.Transaction qualified as Tx
import Servant (
  HasServer (ServerT),
  ServerError,
 )

type ServerM api = ServerT api HandlerM

type HandlerM = Eff EffectStack

type EffectStack = '[Reader AppContext, Error ServerError, IOE]

data AppContext = AppContext
  { _runDBIO :: RunDBIO
  , _tx :: AppTx
  , runDBIO' :: forall a. HSession.Session a -> IO (Either UsageError a)
  , accessId :: ULID
  , reqAt :: POSIXTime
  , loggers :: Loggers
  }

type RunDBIO = forall a. HSession.Session a -> HandlerM a

type AppTx = forall a. Tx.Transaction a -> HandlerM a

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
