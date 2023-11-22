{-# LANGUAGE BlockArguments #-}

module ApiExample.Server (startApp) where

import ApiExample.Framework
import ApiExample.Framework.Types (Http401ErrorRespBody (..))
import ApiExample.GraphQL (GraphQL, handleGql)
import ApiExample.OpenAPI
import Control.Exception (ErrorCall (ErrorCallWithLocation), catch)
import Data.Aeson
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.ULID (getULIDTime)
import Data.Vault.Lazy qualified as Vault
import GHC.IsList (fromList)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.Experimental.Auth
import Servant.Server.Internal.ServerError (responseServerError)
import System.Environment (getEnv)

type App = API :<|> GraphQL

startApp :: IO ()
startApp = do
  port <- read @Int <$> getEnv "SERVER_PORT"
  vaultKey <- Vault.newKey
  vaultAuthKey <- Vault.newKey
  appCtx <- mkAppCtx vaultKey
  let middleware = setUp vaultKey vaultAuthKey . logMiddleware appCtx . catchUnexpectedError appCtx
  let contexts = customFormatters :. authHandler vaultAuthKey :. EmptyContext
  let app = serveWithContext appProxy contexts $ mkServer appCtx
  run port $ middleware app
 where
  appProxy = Proxy @App
  authCtx = Proxy @'[AppAuthHandler]

  mkServer :: AppCtx -> Server App
  mkServer appCtx =
    hoistServerWithContext
      appProxy
      authCtx
      (runHandlerM appCtx)
      (serverM :<|> handleGql)

authHandler :: Vault.Key (Maybe Session) -> AppAuthHandler
authHandler vskey = mkAuthHandler handler
 where
  handler req = case Vault.lookup vskey (vault req) of
    Just (Just session) -> return session
    Nothing -> throwError err500
    _ -> throwError err401{errBody = encode $ toJSON Http401ErrorRespBody{message = "no session"}}

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

catchUnexpectedError :: AppCtx -> Middleware
catchUnexpectedError appCtx app req res = do
  let loggers = extractLoggers . extractReqScopeCtx appCtx $ vault req
  next loggers
 where
  next loggers = catch (app req res) (handlerUnexpectedError loggers)

  handlerUnexpectedError :: Loggers -> ErrorCall -> IO ResponseReceived
  handlerUnexpectedError loggers (ErrorCallWithLocation err locOfErr) = do
    let addionalProps = Just $ fromList [("error", toJSON err), ("locationOfError", toJSON locOfErr)]
        logDanger = logIO loggers Info addionalProps @T.Text
    logDanger msg
    res $ responseServerError err500{errBody = encode $ object ["message" .= msg]}
    
  msg :: T.Text
  msg = "An unexpected error has occurred."

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