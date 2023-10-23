module ApiExample.Framework.AppContext (mkAppCtx) where

import ApiExample.Framework.Types
import Control.Monad.Trans (liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.ULID (getULIDTime)
import Hasql.Pool (Pool, use)
import Hasql.Session qualified as HS
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..), transaction)
import MyLib.Support (getPool)
import Servant (err500, throwError)

mkAppCtx :: IO AppCtx
mkAppCtx = do
  pool <- getPool
  reqAt <- getPOSIXTime
  accessId <- getULIDTime reqAt
  return AppCtx{runDBIO = mkRunnerOfDBIO pool, tx = mkTx pool, accessId, reqAt}

mkRunnerOfDBIO :: Pool -> RunDBIO
mkRunnerOfDBIO pool stmt param = do
  resultOfSQLQuery <- liftIO $ use pool (HS.statement param stmt)
  either (const $ throwError err500) pure resultOfSQLQuery

mkTx :: Pool -> AppTx
mkTx pool mkQueryRunner = do
  resultOfTx <-
    let txQuery = mkQueryRunner $ flip Tx.statement
        resultOfTx = use pool $ transaction RepeatableRead Write txQuery
     in liftIO resultOfTx
  either (\e -> liftIO (print e) *> throwError err500) pure resultOfTx