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
import Hasql.Pool (Pool, UsageError, use)
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

data Session = Session {db :: forall a. HS.Session a -> IO (Either UsageError a)}

type instance AuthServerData (AuthProtect "cookie") = Session
genAuthServerContext :: Context (AuthHandler Request Session ': '[])
genAuthServerContext = authHandler :. EmptyContext

authHandler :: AuthHandler Request Session
authHandler = mkAuthHandler handler
 where
  handler req = case M.lookup "session-id" =<< extractCookies req of
    Just cookie -> do
      pool <- liftIO getPool

      liftIO $ print cookie $> Session{db = use pool}
    _ -> throwError err401

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
startApp = run 8080 $ serveWithContext @API Proxy genAuthServerContext server

server :: Server API
server = handleGetUser :<|> handlePpp

handleGetUser :: Server ListUser
handleGetUser h = liftIO $ print h *> loadUsers

handlePpp :: Server ListUser'
handlePpp Session{db} _ uid = do
  let j = findMany' [uid]
  users <- liftIO $ db j
  case users of
    Right users -> maybe (throwError err404) pure (Vec.headM users)
    Left _ -> throwError err500

loadUsers :: IO [Person]
loadUsers =
  pure
    [ Person{personId = "abcde", age = 20, fullName = "bash"}
    , Person{personId = "fghjk", age = 28, fullName = "zsh"}
    ]

userMap :: IO (M.Map Text Person)
userMap = do
  users <- loadUsers
  return $ M.fromList $ (\u@Person{..} -> (personId, u)) <$> users

findById :: Text -> IO (Maybe Person)
findById n = M.lookup n <$> userMap