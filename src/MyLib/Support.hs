module MyLib.Support where

import Data.Time (secondsToDiffTime)
import Hasql.Connection (settings)
import Hasql.Pool

getPool :: IO Pool
getPool = acquire 10 (secondsToDiffTime 30) (secondsToDiffTime 30 * 5) $ settings "localhost" 5431 "postgres" "postgres" "postgres"

release' :: Pool -> IO ()
release' c = putStrLn "released" *> release c
