module ApiExample.Framework.ReqScopeCtx where

import Data.IORef
import Data.Map
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.ULID (ULID)
import GHC.IO (unsafePerformIO)
import Prelude hiding (lookup)

type GlobalStore = (Map Text ReqScopeCtx)

globalStore :: IORef GlobalStore
{-# NOINLINE globalStore #-}
globalStore = unsafePerformIO $ newIORef mempty

data ReqScopeCtx = ReqScopeCtx
  { accessId :: ULID
  , reqAt :: POSIXTime
  }
  deriving (Show)

readReqScopeCtx :: Text -> IO ReqScopeCtx
readReqScopeCtx key = do
  map <- readIORef globalStore
  case lookup key map of
    Just ctx -> return ctx
    Nothing -> error "aaa"

readReqScopeCtx' :: IO GlobalStore
readReqScopeCtx' = readIORef globalStore

writeReqScopeCtx :: (Text, ReqScopeCtx) -> IO ()
writeReqScopeCtx (k, ctx) = modifyIORef globalStore $ insert k ctx

dropReqScopeCtx :: Text -> IO ()
dropReqScopeCtx k = modifyIORef globalStore $ delete k