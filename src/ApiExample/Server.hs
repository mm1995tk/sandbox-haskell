{-# LANGUAGE BlockArguments #-}

module ApiExample.Server (startApp) where

import ApiExample.Endpoint
import ApiExample.Framework
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.ULID (getULIDTime)
import Data.Vault.Lazy qualified as Vault
import Network.HTTP.Types (status500)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.Experimental.Auth
import System.Environment (getEnv)

startApp :: IO ()
startApp = do
  port <- read @Int <$> getEnv "SERVER_PORT"
  vaultKey <- Vault.newKey
  vaultAuthKey <- Vault.newKey
  appCtx <- mkAppCtx vaultKey
  run port . setUp vaultKey vaultAuthKey . logMiddleware appCtx vaultAuthKey $ serveWithContext api (customFormatters :. authHandler vaultAuthKey :. EmptyContext) (mkServer appCtx)
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

authHandler :: Vault.Key (Maybe Session) -> AppAuthHandler
authHandler vskey = mkAuthHandler handler
 where
  handler req = case Vault.lookup vskey (vault req) of
    Just (Just session) -> return session
    Nothing -> throwError err500
    _ -> throwError err401

setUp :: Vault.Key ReqScopeCtx -> Vault.Key (Maybe Session) -> Middleware
setUp vkey vskey app req res = do
  reqAt <- getPOSIXTime
  accessId <- getULIDTime reqAt
  let vault' = Vault.insert vkey (mkReqScopeCtx accessId reqAt req) (vault req)
  let vault'' = flip (Vault.insert vskey) vault' $ do
        sessionId <- M.lookup "session-id" =<< extractCookies req
        findSession sessionId
  app req{vault = vault''} res
 where
  findSession _ = Just Session{userName = "dummy", email = "dummy"}

logMiddleware :: AppCtx -> Vault.Key (Maybe Session) -> Middleware
logMiddleware appCtx vskey app req res = case Vault.lookup vskey (vault req) of
  Nothing -> res $ responseLBS status500 [] ""
  Just session -> do
    let loggers = extractLoggers . extractReqScopeCtx appCtx $ vault req
    let logInfo = logIO loggers Info (addSessionInfo session) @T.Text
    logInfo "start of request" *> next <* logInfo "end of request"
 where
  next = app req res
  addSessionInfo session = do
    Session{..} <- session
    return [("userName", toJSON userName)]

customFormatters :: ErrorFormatters
customFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = customFormatter
    , notFoundErrorFormatter = notFoundFormatter
    }

customFormatter :: ErrorFormatter
customFormatter tr _ err =
  err400
    { errBody = encode $ object ["combinator" .= show tr, "error" .= err]
    , errHeaders = [("Content-Type", "application/json")]
    }

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter req =
  err404
    { errBody =
        encode
          $ object
            [ "message"
                .= ( "Not found path: " <> decodeUtf8Lenient (rawPathInfo req)
                   )
            ]
    }