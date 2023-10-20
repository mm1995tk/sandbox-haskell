module MyLib.Support where

import Control.Concurrent.Async (concurrently)
import Data.Time (secondsToDiffTime)
import Hasql.Connection (settings)
import Hasql.Pool
import Hasql.Session
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions

-- inConnection :: (Pool -> IO a) -> IO a
-- inConnection f = getPool >>= \pool -> f pool <* release' pool
defaultTx :: Tx.Transaction a -> Session a
defaultTx = transaction RepeatableRead Write

inConnection :: IO (Session a) -> IO (Either UsageError a)
inConnection f = do
  (pool, f') <- concurrently getPool f
  use pool f' <* release' pool

notRelease :: IO (Session a) -> IO (Pool, Either UsageError a)
notRelease f = do
  (pool, f') <- concurrently getPool f
  (pool,) <$> use pool f'

getPool :: IO Pool
getPool = acquire 10 (secondsToDiffTime 30) (secondsToDiffTime 30 * 5) $ settings "localhost" 5431 "postgres" "postgres" "postgres"

-- cleanUpDb :: IO (Either UsageError ())
-- cleanUpDb = inConnection $ statement () $ Statement "truncate person" E.noParams D.noResult True

release' :: Pool -> IO ()
release' c = putStrLn "released" *> release c
