{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module ApiExample.Lib (startApp) where

import ApiExample.Domain (Person (..))
import ApiExample.Infrastructure.Aggregate.Person (findMany')
import Control.Monad.IO.Class (MonadIO (liftIO))
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

type Cookies = [(T.Text, T.Text)]

parseCookies :: T.Text -> Cookies
parseCookies cookieText =
  toTuple . T.splitOn "=" <$> T.splitOn ";" cookieText
 where
  toTuple [key, value] = (key, value)
  toTuple _ = error "Invalid cookie string format"

extractCookies :: Request -> Maybe (M.Map Text Text)
extractCookies req = M.fromList . parseCookies . decodeUtf8Lenient <$> lookup "cookie" (requestHeaders req)

type UseInfra = forall a. HS.Session a -> Handler a
data Session = Session {useInfra :: UseInfra}

type instance AuthServerData (AuthProtect "cookie") = Session
genAuthServerContext :: Pool -> Context (AuthHandler Request Session ': '[])
genAuthServerContext p = authHandler p :. EmptyContext

authHandler :: Pool -> AuthHandler Request Session
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
  p <- getPool
  run 8080 $ serveWithContext @API Proxy (genAuthServerContext p) server

server :: Server API
server = handleGetUsers :<|> handleGetUser

handleGetUsers :: Server ListUser
handleGetUsers h = liftIO $ print h *> loadUsers

handleGetUser :: Server ListUser'
handleGetUser Session{useInfra} _ uid = do
  users <- useInfra (findMany' [uid])
  maybe (throwError err404) pure (Vec.headM users)

loadUsers :: IO [Person]
loadUsers =
  pure
    [ Person{personId = "abcde", age = 20, fullName = "bash"}
    , Person{personId = "fghjk", age = 28, fullName = "zsh"}
    ]
