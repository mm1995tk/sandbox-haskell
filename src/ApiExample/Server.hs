{-# LANGUAGE BlockArguments #-}

module ApiExample.Server (startApp) where

import ApiExample.Endpoint
import ApiExample.Framework
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.ULID (getULIDTime)
import Data.Vault.Lazy qualified as Vault
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.OpenApi (toOpenApi)
import Servant.OpenApi.Internal.Test
import Servant.Server.Experimental.Auth
import System.Environment (getEnv)

outputDoc = BSL8.writeFile "openapi.json" $ encodePretty $ toOpenApi (Proxy @API)

startApp :: IO ()
startApp = do
  port <- read @Int <$> getEnv "SERVER_PORT"
  vaultKey <- Vault.newKey
  vaultAuthKey <- Vault.newKey
  appCtx <- mkAppCtx vaultKey
  let middleware = setUp vaultKey vaultAuthKey . logMiddleware appCtx
  let contexts = customFormatters :. authHandler vaultAuthKey :. EmptyContext
  let app = serveWithContext api contexts $ mkServer appCtx
  run port $ middleware app
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
  let s = extractCookies req >>= M.lookup "session-id" >>= findSession
  let vault' = Vault.insert vkey (mkReqScopeCtx s accessId reqAt req) (vault req)
  let vault'' = Vault.insert vskey s vault'
  app req{vault = vault''} res
 where
  findSession _ = Just Session{userName = "dummy", email = "dummy"}

logMiddleware :: AppCtx -> Middleware
logMiddleware appCtx app req res = do
  let loggers = extractLoggers . extractReqScopeCtx appCtx $ vault req
  let logInfo = logIO loggers Info Nothing @T.Text
  logInfo "start of request" *> next <* logInfo "end of request"
 where
  next = app req res

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