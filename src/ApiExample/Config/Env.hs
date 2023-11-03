module ApiExample.Config.Env (
  dbHost,
  port,
  dbUsername,
  dbPassword,
  database,
  dbPoolSize,
  dbPoolLifetime,
  dbPoolIdletime,
) where

import Data.ByteString.Char8
import Data.ByteString.Conversion (fromByteString)
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import GHC.IO (unsafePerformIO)
import System.Environment (getEnv)
import System.Posix.Env.ByteString qualified as PEB

dbHost :: ByteString
dbHost = getEnv' "HOST"

port :: Word16
port = fromMaybe (error "does not exist (no environment variable)") . fromByteString $ getEnv' "POSTGRES_PORT"

dbUsername :: ByteString
dbUsername = getEnv' "PGUSER"

dbPassword :: ByteString
dbPassword = getEnv' "PGPASSWORD"

database :: ByteString
database = getEnv' "PGDATABASE"

{-# NOINLINE dbPoolSize #-}
dbPoolSize :: Int
dbPoolSize = unsafePerformIO $ read @Int <$> getEnv "CONNECTION_POOL_SIZE"

{-# NOINLINE dbPoolLifetime #-}
dbPoolLifetime :: Integer
dbPoolLifetime = unsafePerformIO $ read @Integer <$> getEnv "MAXIMAL_CONNECTION_LIFETIME"

{-# NOINLINE dbPoolIdletime #-}
dbPoolIdletime :: Integer
dbPoolIdletime = unsafePerformIO $ read @Integer <$> getEnv "MAXIMAL_CONNECTION_IDLE_TIME"

getEnv' :: ByteString -> ByteString
getEnv' s = unsafePerformIO $ fromMaybe (error "does not exist (no environment variable)") <$> PEB.getEnv s
