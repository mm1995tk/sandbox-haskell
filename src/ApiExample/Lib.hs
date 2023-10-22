{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.Lib (startApp) where

import ApiExample.Endpoint
import ApiExample.Framework
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.ByteString.Char8 (unpack)
import Data.Functor (($>))
import Data.Map qualified as M
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixSecondsToUTCTime)
import Data.ULID (ULID, getULIDTime)
import Hasql.Pool (Pool, use)
import Hasql.Session qualified as HS
import Hasql.Transaction qualified as Tx
import MyLib.Support (defaultTx, getPool)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.Experimental.Auth
import System.Environment (getEnv)

startApp :: IO ()
startApp = do
  port <- read @Int <$> getEnv "SERVER_PORT"
  pool <- getPool
  reqAt <- getPOSIXTime
  accessId <- getULIDTime reqAt

  let appCtx = mkAppCtx reqAt accessId pool
  run port . logMiddleware appCtx $ serveWithContext api (authHandler :. EmptyContext) (mkServer appCtx)
 where
  api = Proxy @API
  authCtx = Proxy @'[AppAuthHandler]

  mkServer :: AppCtx -> Server API
  mkServer appCtx =
    hoistServerWithContext
      api
      authCtx
      (`runReaderT` appCtx)
      serverM

  mkAppCtx :: POSIXTime -> ULID -> Pool -> AppCtx
  mkAppCtx reqAt accessId pool = AppCtx{runDBIO = mkRunnerOfDBIO pool, tx = mkTx pool, accessId, reqAt}

  mkRunnerOfDBIO :: Pool -> RunDBIO
  mkRunnerOfDBIO pool stmt param = liftIO (use pool (HS.statement param stmt)) >>= either (const $ throwError err500) pure

  mkTx :: Pool -> AppTx
  mkTx pool mkQueryRunner = do
    resultOfTx <-
      let txQuery = mkQueryRunner $ flip Tx.statement
          resultOfTx = use pool $ defaultTx txQuery
       in liftIO resultOfTx
    either (\e -> liftIO (print e) *> throwError err500) pure resultOfTx

authHandler :: AppAuthHandler
authHandler = mkAuthHandler handler
 where
  handler req = case M.lookup "session-id" =<< extractCookies req of
    Just sessionId -> liftIO $ mkSession sessionId
    _ -> throwError err401

  mkSession sessionId = print ("sessionId: " <> sessionId) $> Session{userName = "dummy", email = "dummy"}

logMiddleware :: AppCtx -> Middleware
logMiddleware AppCtx{reqAt, accessId} app req res = do
  putStrLn "hello"
  print $ posixSecondsToUTCTime reqAt
  print accessId
  putStrLn (unpack method)
  next <* putStrLn "bye"
 where
  next = app req res
  method = requestMethod req