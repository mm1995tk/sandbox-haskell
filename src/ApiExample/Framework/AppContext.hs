module ApiExample.Framework.AppContext (
  mkAppCtx,
  runDBIO,
  transaction,
  mkReqScopeCtx,
  extractReqScopeCtx,
  extractLoggers,
) where

import ApiExample.Framework.Logger (logM, mkLogger)
import ApiExample.Framework.Server
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
      , runDBIO' = use pool
      , _tx = mkTx pool
      , _reqScopeCtx = fromMaybe (error "the vault key is not found.") . Vault.lookup vaultKey
      }

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

runDBIO :: HSession.Session a -> HandlerWithReqScopeCtx a
runDBIO s = join $ asks runDBIO' <*> pure s
 where
  runDBIO' AppCtx{_runDBIO} = _runDBIO

transaction :: Tx.Transaction a -> HandlerWithReqScopeCtx a
transaction s = join $ asks tx' <*> pure s
 where
  tx' AppCtx{_tx} = _tx

extractReqScopeCtx :: AppCtx -> Vault.Vault -> ReqScopeCtx
extractReqScopeCtx AppCtx{_reqScopeCtx} = _reqScopeCtx

extractLoggers :: ReqScopeCtx -> Loggers
extractLoggers ReqScopeCtx{loggers} = loggers
