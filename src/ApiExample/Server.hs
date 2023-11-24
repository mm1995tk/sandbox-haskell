{-# LANGUAGE BlockArguments #-}

module ApiExample.Server (startApp) where

import ApiExample.Config.Key (keyOfSessionId)
import ApiExample.Framework
import ApiExample.GraphQL (GraphQL, handleGql)
import ApiExample.OpenAPI
import Control.Exception (ErrorCall (ErrorCallWithLocation), catch)
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Coerce (coerce)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixSecondsToUTCTime)
import Data.ULID (ULID, getULIDTime)
import Data.Vault.Lazy qualified as Vault
import Effectful (liftIO, runEff)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Dynamic (runErrorNoCallStack)
import Effectful.Error.Dynamic qualified as Effectful
import Effectful.Reader.Dynamic (runReader)
import Examples.HasqlExample (getPool)
import GHC.IsList (fromList)
import Hasql.Pool (Pool, use)
import Hasql.Transaction.Sessions qualified as Txs
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
  let appCtx = mkAppCtx vaultKey
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

runHandlerM :: AppCtx -> HandlerM a -> Handler a
runHandlerM ctx e = liftIO (run' e) >>= either Servant.throwError pure
 where
  run' = runEff . runErrorNoCallStack @ServerError . runWrappedHandler . runReader ctx

  runWrappedHandler = interpret handler
   where
    handler _ (WrapHandler h) = liftIO (runHandler h) >>= either Effectful.throwError pure

authHandler :: Vault.Key (Maybe Session) -> AppAuthHandler
authHandler vskey = mkAuthHandler handler
 where
  handler req = case Vault.lookup vskey (vault req) of
    Just (Just session) -> return session
    Nothing -> throwError err500
    _ -> throwError err401{errBody = encode $ toJSON Http401ErrorRespBody{message = "no session"}}

setUp :: Vault.Key ReqScopeCtx -> Vault.Key (Maybe Session) -> Middleware
setUp vkey vskey app req res = do
  pool <- getPool
  reqAt <- getPOSIXTime
  accessId <- getULIDTime reqAt
  let s = extractCookies req >>= M.lookup keyOfSessionId >>= findSession
  let vault' = Vault.insert vkey (mkReqScopeCtx pool s accessId reqAt req) (vault req)
  let vault'' = Vault.insert vskey s vault'
  app req{vault = vault''} res
 where
  findSession _ = Just Session{userName = "dummy", email = "dummy"}

catchUnexpectedError :: AppCtx -> Middleware
catchUnexpectedError appCtx app req res = do
  let loggers = extractLoggers . coerce appCtx $ vault req
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
  let loggers = extractLoggers . coerce appCtx $ vault req
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

mkAppCtx :: Vault.Key ReqScopeCtx -> AppCtx
mkAppCtx vaultKey = AppCtx (fromMaybe (error "the vault key is not found.") . Vault.lookup vaultKey)

mkReqScopeCtx :: Pool -> Maybe Session -> ULID -> POSIXTime -> Request -> ReqScopeCtx
mkReqScopeCtx pool s accessId reqAt req =
  ReqScopeCtx
    { _runDBIO = mkRunnerOfDBIO pool
    , runDBIO' = use pool
    , _tx = mkTx pool
    , accessId
    , reqAt
    , loggers = mkLoggers (mkLogger s accessId reqAt req)
    }
 where
  mkLoggers :: (LogLevel -> Logger) -> Loggers
  mkLoggers logger =
    Loggers
      { danger = logger Danger
      , warn = logger Warning
      , info = logger Info
      }

mkRunnerOfDBIO :: Pool -> RunDBIO
mkRunnerOfDBIO pool s = do
  let logDanger = logM Danger Nothing @String
  resultOfSQLQuery <- liftIO $ use pool s
  either (\e -> logDanger (show e) *> Effectful.throwError err500) pure resultOfSQLQuery

mkTx :: Pool -> AppTx
mkTx pool txQuery = do
  resultOfTx <-
    let resultOfTx = use pool $ Txs.transaction Txs.RepeatableRead Txs.Write txQuery
     in liftIO resultOfTx
  let logDanger = logM Danger Nothing @String
  either (\e -> logDanger (show e) *> Effectful.throwError err500) pure resultOfTx

mkLogger :: Maybe Session -> ULID -> POSIXTime -> Request -> LogLevel -> Logger
mkLogger s accessId reqAt req loglevel additionalProps' item = BS.putStrLn . encode $ jsonLog <> sessionInfo <> additionalProps
 where
  jsonLog =
    fromList
      [ ("level", toJSON loglevel)
      , ("accessId", toJSON $ show accessId)
      , ("method", method)
      , ("path", path)
      , ("reqAt", toJSON $ posixSecondsToUTCTime reqAt)
      , ("message", toJSON item)
      , ("remoteHost", remoteHostAddr)
      , ("queryParams", queryParams)
      ]

  sessionInfo :: KeyMap Value
  sessionInfo = case s of
    Nothing -> mempty
    Just Session{..} -> fromList [("userName", toJSON userName)]

  additionalProps = maybe mempty fromList additionalProps'
  path = toJSON . decodeUtf8Lenient $ rawPathInfo req
  method = toJSON . decodeUtf8Lenient $ requestMethod req
  remoteHostAddr = toJSON . show $ remoteHost req
  queryParams = toJSON . show $ queryString req

extractLoggers :: ReqScopeCtx -> Loggers
extractLoggers ReqScopeCtx{loggers} = loggers