module ApiExample.Framework.AppContext (mkAppCtx) where

import ApiExample.Framework.Types
import Control.Monad.Trans (liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.ULID (getULIDTime)
import Hasql.Pool (Pool, use)
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
mkRunnerOfDBIO pool s = do
  resultOfSQLQuery <- liftIO $ use pool s
  either (const $ throwError err500) pure resultOfSQLQuery

mkTx :: Pool -> AppTx
mkTx pool txQuery = do
  resultOfTx <-
    let resultOfTx = use pool $ transaction RepeatableRead Write txQuery
     in liftIO resultOfTx
  either (\e -> liftIO (print e) *> throwError err500) pure resultOfTx