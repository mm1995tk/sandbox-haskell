{-# LANGUAGE BlockArguments #-}

module ApiExample.Server (startApp) where

import ApiExample.Endpoint
import ApiExample.Framework
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson
import Data.ByteString.Char8 qualified as BS
import Data.Functor (($>))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.ULID (getULIDTime)
import Data.Vault.Lazy qualified as Vault
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.Experimental.Auth
import System.Environment (getEnv)

startApp :: IO ()
startApp = do
  port <- read @Int <$> getEnv "SERVER_PORT"
  vaultKey <- Vault.newKey
  appCtx <- mkAppCtx vaultKey
  run port . setUpGlobalStore vaultKey . logMiddleware appCtx $ serveWithContext api contexts (mkServer appCtx)
 where
  api = Proxy @API
  authCtx = Proxy @'[AppAuthHandler]
  contexts = customFormatters :. authHandler :. EmptyContext

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

setUpGlobalStore :: Vault.Key ReqScopeCtx -> Middleware
setUpGlobalStore vkey app req res = do
  reqAt <- getPOSIXTime
  accessId <- getULIDTime reqAt
  let cur = vault req
  let k = T.pack . show $ accessId
  let logging :: (LogLevel -> Logger) = mkLogger accessId reqAt

  let vault' =
        Vault.insert
          vkey
          ( ReqScopeCtx
              { accessId
              , reqAt
              , loggers =
                  Loggers
                    { danger = logging Danger
                    , warn = logging Warning
                    , info = logging Info
                    }
              }
          )
          cur
  next k vault'
 where
  next k vault' = app req{requestHeaders = ("x-custom-accessId", encodeUtf8 k) : requestHeaders req, vault = vault'} res

logMiddleware :: AppCtx -> Middleware
logMiddleware AppCtx{reqScopeCtx} app req res = do
  let ReqScopeCtx{..} = reqScopeCtx $ vault req
  let Loggers{info} = loggers
  let info' = info @T.Text
  info' "start of request" *> next <* info' "end of request"
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