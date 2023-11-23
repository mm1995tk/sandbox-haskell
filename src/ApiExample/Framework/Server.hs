{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.Framework.Server where

import ApiExample.Framework.Security (Session (..))
import Data.Aeson (Key, ToJSON (..), Value, encode)
import Data.Aeson.KeyMap (KeyMap)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.ULID (ULID)
import Data.Vault.Lazy qualified as Vault
import Effectful (Eff, Effect, IOE, liftIO, runEff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Dynamic (Error, runError, throwError)
import Effectful.Error.Dynamic qualified as Effectful
import Effectful.Reader.Dynamic
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import GHC.IsList (fromList)
import Hasql.Pool (Pool, UsageError, use)
import Hasql.Session qualified as HSession
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as Txs
import Network.Wai (Request (queryString, rawPathInfo, remoteHost, requestMethod))
import Servant hiding (throwError, (:>))
import Servant qualified

data WrappedHandler :: Effect where
  WrapHandler :: (Handler a) -> WrappedHandler m a

makeEffect ''WrappedHandler

data RaiseTransaction :: Effect where
  RaiseTransaction :: (Tx.Transaction a) -> RaiseTransaction m a

makeEffect ''RaiseTransaction

runWrappedHandler :: (IOE :> es, Error ServerError :> es) => Eff (WrappedHandler : es) a -> Eff es a
runWrappedHandler = interpret handler
 where
  handler _ (WrapHandler h) = liftIO (runHandler h) >>= either Effectful.throwError pure

runHandlerM :: AppCtx -> HandlerM a -> Handler a
runHandlerM ctx e = liftIO (run e) >>= either (Servant.throwError . snd) pure
 where
  run = runEff . runError @ServerError . runWrappedHandler . runReader ctx

runReaderReqScopeCtx :: HandlerWithReqScopeCtx a -> Vault.Vault -> HandlerM a
runReaderReqScopeCtx h v = do
  f <- coerce <$> ask
  runReader (f v) h

runReaderReqScopeCtx' :: Vault -> HandlerWithReqScopeCtx a -> HandlerM a
runReaderReqScopeCtx' = flip runReaderReqScopeCtx

runTx :: TxHandler a -> HandlerWithReqScopeCtx a
runTx = interpret handler
 where
  handler _ (RaiseTransaction tx) = transaction tx

type ServerM api = ServerT api HandlerM

type BaseEffectStack = '[Reader AppCtx, WrappedHandler, Error ServerError, IOE]

type HandlerM = Eff BaseEffectStack

type HandlerWithReqScopeCtx = Eff (Reader ReqScopeCtx : BaseEffectStack)

type TxHandler = Eff (RaiseTransaction : Reader ReqScopeCtx : BaseEffectStack)

type RunDBIO = forall a. HSession.Session a -> HandlerWithReqScopeCtx a

type AppTx = forall a. Tx.Transaction a -> HandlerWithReqScopeCtx a

newtype AppCtx = AppCtx (Vault.Vault -> ReqScopeCtx)

data ReqScopeCtx = ReqScopeCtx
  { _runDBIO :: RunDBIO
  , _tx :: AppTx
  , runDBIO' :: forall a. HSession.Session a -> IO (Either UsageError a)
  , accessId :: ULID
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

mkAppCtx :: Vault.Key ReqScopeCtx -> AppCtx
mkAppCtx vaultKey = AppCtx (fromMaybe (error "the vault key is not found.") . Vault.lookup vaultKey)

mkRunnerOfDBIO :: Pool -> RunDBIO
mkRunnerOfDBIO pool s = do
  let logDanger = logM Danger Nothing @String
  resultOfSQLQuery <- liftIO $ use pool s
  either (\e -> logDanger (show e) *> throwError err500) pure resultOfSQLQuery

mkTx :: Pool -> AppTx
mkTx pool txQuery = do
  resultOfTx <-
    let resultOfTx = use pool $ Txs.transaction Txs.RepeatableRead Txs.Write txQuery
     in liftIO resultOfTx
  let logDanger = logM Danger Nothing @String
  either (\e -> logDanger (show e) *> throwError err500) pure resultOfTx

mkReqScopeCtx :: Pool -> Maybe Session -> ULID -> POSIXTime -> Request -> ReqScopeCtx
mkReqScopeCtx pool s accessId reqAt req =
  ReqScopeCtx
    { _runDBIO = mkRunnerOfDBIO pool
    , runDBIO' = use pool
    , _tx = mkTx pool
    , accessId
    , reqAt
    , loggers = mkLoggers (mkLogger s accessId reqAt req)
    }
 where
  mkLoggers :: (LogLevel -> Logger) -> Loggers
  mkLoggers logger =
    Loggers
      { danger = logger Danger
      , warn = logger Warning
      , info = logger Info
      }

runDBIO :: HSession.Session a -> HandlerWithReqScopeCtx a
runDBIO s = do
  ReqScopeCtx{_runDBIO} <- ask
  _runDBIO s

transaction :: Tx.Transaction a -> HandlerWithReqScopeCtx a
transaction s = do
  ReqScopeCtx{_tx} <- ask
  _tx s

extractReqScopeCtx :: AppCtx -> Vault.Vault -> ReqScopeCtx
extractReqScopeCtx = coerce

extractLoggers :: ReqScopeCtx -> Loggers
extractLoggers ReqScopeCtx{loggers} = loggers

mkLogger :: Maybe Session -> ULID -> POSIXTime -> Request -> LogLevel -> Logger
mkLogger s accessId reqAt req loglevel additionalProps' item = BS.putStrLn . encode $ jsonLog <> sessionInfo <> additionalProps
 where
  jsonLog =
    fromList
      [ ("level", toJSON loglevel)
      , ("accessId", toJSON $ show accessId)
      , ("method", method)
      , ("path", path)
      , ("reqAt", toJSON $ posixSecondsToUTCTime reqAt)
      , ("message", toJSON item)
      , ("remoteHost", remoteHostAddr)
      , ("queryParams", queryParams)
      ]

  sessionInfo :: KeyMap Value
  sessionInfo = case s of
    Nothing -> mempty
    Just Session{..} -> fromList [("userName", toJSON userName)]

  additionalProps = maybe mempty fromList additionalProps'
  path = toJSON . decodeUtf8Lenient $ rawPathInfo req
  method = toJSON . decodeUtf8Lenient $ requestMethod req
  remoteHostAddr = toJSON . show $ remoteHost req
  queryParams = toJSON . show $ queryString req

logM :: LogLevel -> Maybe [(Key, Value)] -> forall a. (Show a, ToJSON a) => a -> HandlerWithReqScopeCtx ()
logM level customProps msg = do
  ReqScopeCtx{loggers} <- ask
  liftIO $ logIO loggers level customProps msg

logIO :: Loggers -> LogLevel -> Logger
logIO Loggers{..} = \case
  Danger -> danger
  Warning -> warn
  Info -> info
