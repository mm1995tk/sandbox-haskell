{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.Lib (startApp) where

import ApiExample.Domain (Person (..))
import ApiExample.Infrastructure.Aggregate.Person (findAll, findMany')
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Functor (($>))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixSecondsToUTCTime)
import Data.ULID (ULID, getULIDTime)
import Data.Vector qualified as Vec
import Hasql.Pool (Pool, use)
import Hasql.Session qualified as HS
import Hasql.Statement qualified as Hstmt
import Hasql.Transaction qualified as Tx
import MyLib.Support (defaultTx, getPool)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.Experimental.Auth
import System.Environment (getEnv)

type HandlerM = ReaderT AppCtx Handler

type RunDBIO = forall param result. Hstmt.Statement param result -> param -> HandlerM result

type AppTx = forall a. ((forall param result. Hstmt.Statement param result -> param -> Tx.Transaction result) -> Tx.Transaction a) -> HandlerM a

data AppCtx = AppCtx
  { runDBIO :: RunDBIO
  , tx :: AppTx
  , accessId :: ULID
  , reqAt :: POSIXTime
  }

type ServerM api = ServerT api HandlerM

type Cookies = [(T.Text, T.Text)]

parseCookies :: T.Text -> Cookies
parseCookies cookieText =
  toTuple . T.splitOn "=" <$> T.splitOn ";" cookieText
 where
  toTuple [key, value] = (key, value)
  toTuple _ = error "Invalid cookie string format"

extractCookies :: Request -> Maybe (M.Map Text Text)
extractCookies req = M.fromList . parseCookies . decodeUtf8Lenient <$> lookup "cookie" (requestHeaders req)

data Session = Session {userName :: Text, email :: Text}

type CookieAuth = AuthProtect "cookie"

type instance AuthServerData CookieAuth = Session
genAuthServerContext :: Context (AuthHandler Request Session ': '[])
genAuthServerContext = authHandler :. EmptyContext

type AppAuthHandler = AuthHandler Request Session

authHandler :: AppAuthHandler
authHandler = mkAuthHandler handler
 where
  handler req = case M.lookup "session-id" =<< extractCookies req of
    Just sessionId -> liftIO $ mkSession sessionId
    _ -> throwError err401

  mkSession sessionId = print ("sessionId: " <> sessionId) $> Session{userName = "dummy", email = "dummy"}

type API = ListUser :<|> ListUser'

type ListUser =
  "users"
    :> Header "user-agent" Text
    :> Get '[JSON] (Vec.Vector Person)

type ListUser' =
  "users"
    :> CookieAuth
    :> Header "user-agent" Text
    :> Capture "ppp" Text
    :> Get '[JSON] Person

$(deriveJSON defaultOptions ''Person)

startApp :: IO ()
startApp = do
  port <- read @Int <$> getEnv "SERVER_PORT"
  pool <- getPool
  reqAt <- getPOSIXTime
  accessId <- getULIDTime reqAt

  let appCtx = mkAppCtx reqAt accessId pool
  run port . logMiddleware appCtx $ serveWithContext api genAuthServerContext (mkServer appCtx)
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

serverM :: ServerM API
serverM = handleGetUsers :<|> handleGetUser

handleGetUsers :: ServerM ListUser
handleGetUsers _ = do
  AppCtx{runDBIO} <- ask
  runDBIO findAll ()

handleGetUser :: ServerM ListUser'
handleGetUser Session{userName} _ uid = do
  AppCtx{runDBIO} <- ask
  liftIO $ print userName
  users <- runDBIO findMany' [uid]
  maybe (throwError err404) pure (Vec.headM users)

logMiddleware :: AppCtx -> Middleware
logMiddleware AppCtx{reqAt, accessId} app req res = do
  print $ posixSecondsToUTCTime reqAt
  print accessId
  putStrLn "hello"
  print method
  next <* putStrLn "bye"
 where
  next = app req res
  method = requestMethod req