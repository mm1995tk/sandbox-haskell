module ApiExample.Endpoint.GetUser where

import ApiExample.Domain (Person)
import ApiExample.Framework (AppCtx (..), CookieAuth, ServerM, Session (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (..), ask)

import ApiExample.Infrastructure (findMany)
import Control.Monad.Error.Class (throwError)
import Data.Text (Text)
import Data.Vector qualified as Vec
import Servant (Capture, Get, Header, JSON, (:>))
import Servant.Server (err404)

type GetUser =
  "users"
    :> CookieAuth
    :> Header "user-agent" Text
    :> Capture "ppp" Text
    :> Get '[JSON] Person

handleGetUser :: ServerM GetUser
handleGetUser Session{userName} _ uid = do
  AppCtx{runDBIO} <- ask
  liftIO $ print userName
  users <- runDBIO findMany [uid]
  maybe (throwError err404) pure (Vec.headM users)
