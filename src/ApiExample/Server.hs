{-# LANGUAGE BlockArguments #-}

module ApiExample.Server (startApp) where

import ApiExample.Endpoint
import ApiExample.Framework
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson
import Data.Functor (($>))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time.Clock.POSIX (getPOSIXTime)
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
  run port . setUp vaultKey . logMiddleware appCtx $ serveWithContext api contexts (mkServer appCtx)
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

setUp :: Vault.Key ReqScopeCtx -> Middleware
setUp vkey app req res = do
  reqAt <- getPOSIXTime
  accessId <- getULIDTime reqAt
  let vault' = updateVault accessId reqAt $ vault req
  next vault'
 where
  next vault' = app req{vault = vault'} res

  updateVault accessId reqAt =
    Vault.insert
      vkey
      ReqScopeCtx
        { accessId
        , reqAt
        , loggers = mkLoggers (mkLogger accessId reqAt req)
        }

  mkLoggers :: (LogLevel -> Logger) -> Loggers
  mkLoggers logger =
    Loggers
      { danger = logger Danger
      , warn = logger Warning
      , info = logger Info
      }

logMiddleware :: AppCtx -> Middleware
logMiddleware AppCtx{_reqScopeCtx} app req res = do
  let ReqScopeCtx{loggers} = _reqScopeCtx $ vault req
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