module ApiExample.Framework.AppContext (mkAppCtx, runDBIO, transaction) where

import ApiExample.Framework.Types
import Control.Monad (join)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import Data.Vault.Lazy qualified as Vault
import Hasql.Pool (Pool, use)
import Hasql.Session qualified as HSession
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as Txs
import MyLib.Support (getPool)
import Servant (err500, throwError)

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
  either (const $ throwError err500) pure resultOfSQLQuery

mkTx :: Pool -> AppTx
mkTx pool txQuery = do
  resultOfTx <-
    let resultOfTx = use pool $ Txs.transaction Txs.RepeatableRead Txs.Write txQuery
     in liftIO resultOfTx
  either (\e -> liftIO (print e) *> throwError err500) pure resultOfTx

runDBIO :: HSession.Session a -> HandlerM a
runDBIO s = join $ asks runDBIO' <*> pure s
 where
  runDBIO' AppCtx{_runDBIO} = _runDBIO

transaction :: Tx.Transaction a -> HandlerM a
transaction s = join $ asks tx' <*> pure s
 where
  tx' AppCtx{_tx} = _tx
