{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ApiExample.Lib (startApp) where

import ApiExample.Domain (Person (..))
import ApiExample.Infrastructure.Aggregate.Person (findMany')
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Functor (($>))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Vector qualified as Vec
import Hasql.Pool (Pool, use)
import Hasql.Session qualified as HS
import MyLib.Support (getPool)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.Experimental.Auth
import System.Environment (getEnv)

type HandlerM = ReaderT AppCtx Handler

data AppCtx = AppCtx deriving (Show)

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

type UseInfra = forall a. HS.Session a -> HandlerM a
data Session = Session {useInfra :: UseInfra}

type instance AuthServerData (AuthProtect "cookie") = Session
genAuthServerContext :: Pool -> Context (AuthHandler Request Session ': '[])
genAuthServerContext p = authHandler p :. EmptyContext

type AppAuthHandler = AuthHandler Request Session

authHandler :: Pool -> AppAuthHandler
authHandler pool = mkAuthHandler handler
 where
  handler req = case M.lookup "session-id" =<< extractCookies req of
    Just sessionId -> liftIO $ mkSession sessionId
    _ -> throwError err401

  mkSession sessionId = print ("sessionId: " <> sessionId) $> Session{useInfra}

  useInfra :: UseInfra
  useInfra s = liftIO (use pool s) >>= either (const $ throwError err500) pure

type API = ListUser :<|> ListUser'

type ListUser =
  "users"
    :> Header "user-agent" Text
    :> Get '[JSON] [Person]

type ListUser' =
  "users"
    :> AuthProtect "cookie"
    :> Header "user-agent" Text
    :> Capture "ppp" Text
    :> Get '[JSON] Person

$(deriveJSON defaultOptions ''Person)

startApp :: IO ()
startApp = do
  port <- read @Int <$> getEnv "SERVER_PORT"
  ctx <- genAuthServerContext <$> getPool
  run port $ serveWithContext api ctx server
 where
  api = Proxy @API
  authCtx = Proxy @'[AppAuthHandler]

  runHandlerM :: HandlerM a -> Handler a
  runHandlerM handlerM = runReaderT handlerM AppCtx

  server :: Server API
  server = hoistServerWithContext api authCtx runHandlerM serverM

serverM :: ServerM API
serverM = handleGetUsers :<|> handleGetUser

handleGetUsers :: ServerM ListUser
handleGetUsers h = liftIO $ print h *> loadUsers

handleGetUser :: ServerM ListUser'
handleGetUser Session{useInfra} _ uid = do
  ctx <- ask
  liftIO $ print ctx
  users <- useInfra (findMany' [uid])
  maybe (throwError err404) pure (Vec.headM users)

loadUsers :: IO [Person]
loadUsers =
  pure
    [ Person{personId = "abcde", age = 20, fullName = "bash"}
    , Person{personId = "fghjk", age = 28, fullName = "zsh"}
    ]
