module ApiExample.Framework.AppContext (mkAppCtx) where

import ApiExample.Framework.Types
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import Data.Vault.Lazy qualified as Vault
import Hasql.Pool (Pool, use)
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..), transaction)
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
    let resultOfTx = use pool $ transaction RepeatableRead Write txQuery
     in liftIO resultOfTx
  either (\e -> liftIO (print e) *> throwError err500) pure resultOfTx