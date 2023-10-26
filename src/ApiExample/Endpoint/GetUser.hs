module ApiExample.Endpoint.GetUser where

import ApiExample.Domain (Person)
import ApiExample.Framework (AppCtx (..), CookieAuth, ServerM, Session (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (..), ask)

import ApiExample.Infrastructure (findMany)
import Control.Monad.Error.Class (throwError)
import Data.Text (Text)
import Data.Vector qualified as Vec
import Servant (Capture, Get, Header, JSON, ServerError (errBody), (:>))
import Servant.Server (err404)

type GetUser =
  "users"
    :> CookieAuth
    :> Header "user-agent" Text
    :> Capture "user-id" Text
    :> Get '[JSON] Person

handleGetUser :: ServerM GetUser
handleGetUser Session{userName} _ uid = do
  AppCtx{runDBIO} <- ask
  liftIO $ print userName
  users <- runDBIO findMany [uid]
  if Vec.null users then throwError err404{errBody = "the user you specified is not found"} else return (Vec.head users)
