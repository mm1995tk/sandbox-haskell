module MyLib.Support where

import ApiExample.Config.Env (database, dbConnectionTimeout, dbHost, dbPassword, dbPoolIdletime, dbPoolLifetime, dbPoolSize, dbUsername, port)
import Data.Time (secondsToDiffTime)
import Hasql.Connection (settings)
import Hasql.Pool

getPool :: IO Pool
getPool =
  acquire
    dbPoolSize
    (secondsToDiffTime dbConnectionTimeout)
    (secondsToDiffTime dbPoolLifetime)
    (secondsToDiffTime dbPoolIdletime)
    (settings dbHost port dbUsername dbPassword database)

release' :: Pool -> IO ()
release' c = putStrLn "released" *> release c
