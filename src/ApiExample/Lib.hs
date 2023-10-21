{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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

type UseInfra = forall param result. Hstmt.Statement param result -> param -> HandlerM result

data AppCtx = AppCtx {useInfra :: UseInfra, tx :: forall a. Tx.Transaction a -> HandlerM a}

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

data Session = Session {userName :: Text}

type instance AuthServerData (AuthProtect "cookie") = Session
genAuthServerContext :: Context (AuthHandler Request Session ': '[])
genAuthServerContext = authHandler :. EmptyContext

type AppAuthHandler = AuthHandler Request Session

authHandler :: AppAuthHandler
authHandler = mkAuthHandler handler
 where
  handler req = case M.lookup "session-id" =<< extractCookies req of
    Just sessionId -> liftIO $ mkSession sessionId
    _ -> throwError err401

  mkSession sessionId = print ("sessionId: " <> sessionId) $> Session{userName = "dummy"}

type API = ListUser :<|> ListUser'

type ListUser =
  "users"
    :> Header "user-agent" Text
    :> Get '[JSON] (Vec.Vector Person)

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
  pool <- getPool
  let server =
        hoistServerWithContext
          api
          authCtx
          ( `runReaderT`
              AppCtx
                { useInfra = mkUserInfra pool
                , tx = \t -> liftIO (use pool (defaultTx t)) >>= either (const $ throwError err500) pure
                }
          )
          serverM
  run port $ serveWithContext api genAuthServerContext server
 where
  api = Proxy @API
  authCtx = Proxy @'[AppAuthHandler]

  mkUserInfra :: Pool -> UseInfra
  mkUserInfra pool stmt param = liftIO (use pool (HS.statement param stmt)) >>= either (const $ throwError err500) pure

serverM :: ServerM API
serverM = handleGetUsers :<|> handleGetUser

handleGetUsers :: ServerM ListUser
handleGetUsers _ = do
  AppCtx{useInfra} <- ask
  useInfra findAll ()

handleGetUser :: ServerM ListUser'
handleGetUser Session{userName} _ uid = do
  AppCtx{useInfra} <- ask
  liftIO $ print userName
  users <- useInfra findMany' [uid]
  maybe (throwError err404) pure (Vec.headM users)
