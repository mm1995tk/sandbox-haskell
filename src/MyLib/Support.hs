module MyLib.Support where

import Data.Maybe (fromMaybe)
import Data.Time (secondsToDiffTime)
import GHC.Word (Word16)
import Hasql.Connection (settings)
import Hasql.Pool
import System.Environment (getEnv)
import System.Posix.Env.ByteString qualified as PEB

getPool :: IO Pool
getPool = do
  host <- getEnv' "HOST"
  port <- read @Word16 <$> getEnv "POSTGRES_PORT"
  user <- getEnv' "PGUSER"
  password <- getEnv' "PGPASSWORD"
  database <- getEnv' "PGDATABASE"
  poolSize <- read @Int <$> getEnv "CONNECTION_POOL_SIZE"
  lifetime <- read @Integer <$> getEnv "MAXIMAL_CONNECTION_LIFETIME"
  idletime <- read @Integer <$> getEnv "MAXIMAL_CONNECTION_IDLE_TIME"

  acquire poolSize (secondsToDiffTime lifetime) (secondsToDiffTime idletime) $ settings host port user password database
 where
  getEnv' s = fromMaybe (error "does not exist (no environment variable)") <$> PEB.getEnv s

release' :: Pool -> IO ()
release' c = putStrLn "released" *> release c
