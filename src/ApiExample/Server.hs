{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.Server (startApp) where

import ApiExample.Endpoint
import ApiExample.Framework
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.ByteString.Char8 (unpack)
import Data.Functor (($>))
import Data.Map qualified as M
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.Experimental.Auth
import System.Environment (getEnv)

startApp :: IO ()
startApp = do
  port <- read @Int <$> getEnv "SERVER_PORT"
  appCtx <- mkAppCtx
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