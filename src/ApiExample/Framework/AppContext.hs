module ApiExample.Framework.AppContext (
  mkAppCtx,
  runDBIO,
  transaction,
  getAccessId,
  getReqAt,
  mkReqScopeCtx,
  extractReqScopeCtx,
  extractLoggers,
) where

import ApiExample.Framework.Logger (mkLogger)
import ApiExample.Framework.Types
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.ULID (ULID)
import Data.Vault.Lazy qualified as Vault
import Effectful (liftIO)
import Effectful.Error.Dynamic
import Effectful.Reader.Dynamic (asks)
import Hasql.Pool (Pool, use)
import Hasql.Session qualified as HSession
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as Txs
import MyLib.Support (getPool)
import Network.Wai (Request)
import Servant (err500)

mkAppCtx :: Vault.Key ReqScopeCtx -> IO AppCtx
mkAppCtx vaultKey = do
  pool <- getPool
  return
    AppCtx
      { _runDBIO = mkRunnerOfDBIO pool
      , _tx = mkTx pool
      , _reqScopeCtx = fromMaybe (error "the vault key is not found.") . Vault.lookup vaultKey
      }

mkRunnerOfDBIO :: Pool -> RunDBIO
mkRunnerOfDBIO pool s = do
  resultOfSQLQuery <- liftIO $ use pool s
  -- TODO: きちんとログに出す。エラーから復帰したい場合の処理を考える
  either (\e -> liftIO (print e) *> throwError err500) pure resultOfSQLQuery

mkTx :: Pool -> AppTx
mkTx pool txQuery = do
  resultOfTx <-
    let resultOfTx = use pool $ Txs.transaction Txs.RepeatableRead Txs.Write txQuery
     in liftIO resultOfTx
  -- TODO: きちんとログに出す。エラーから復帰したい場合の処理を考える
  either (\e -> liftIO (print e) *> throwError err500) pure resultOfTx

mkReqScopeCtx :: Maybe Session -> ULID -> POSIXTime -> Request -> ReqScopeCtx
mkReqScopeCtx s accessId reqAt req =
  ReqScopeCtx
    { accessId
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

runDBIO :: HSession.Session a -> HandlerM a
runDBIO s = join $ asks runDBIO' <*> pure s
 where
  runDBIO' AppCtx{_runDBIO} = _runDBIO

transaction :: Tx.Transaction a -> HandlerM a
transaction s = join $ asks tx' <*> pure s
 where
  tx' AppCtx{_tx} = _tx

getAccessId :: Vault.Vault -> HandlerM ULID
getAccessId v = do
  reqScopeCtx <- asks extractReqScopeCtx
  let ReqScopeCtx{accessId} = reqScopeCtx v
  return accessId

getReqAt :: Vault.Vault -> HandlerM POSIXTime
getReqAt v = do
  reqScopeCtx <- asks extractReqScopeCtx
  let ReqScopeCtx{reqAt} = reqScopeCtx v
  return reqAt

extractReqScopeCtx :: AppCtx -> Vault.Vault -> ReqScopeCtx
extractReqScopeCtx AppCtx{_reqScopeCtx} = _reqScopeCtx

extractLoggers :: ReqScopeCtx -> Loggers
extractLoggers ReqScopeCtx{loggers} = loggers
