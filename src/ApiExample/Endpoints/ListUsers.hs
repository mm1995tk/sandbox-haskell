module ApiExample.Endpoints.ListUsers where

import ApiExample.Domain (Person)
import ApiExample.Framework (AppCtx (..), CookieAuth, ServerM, Session (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (..), ask)

import ApiExample.Infrastructure (findAll, findMany')
import Control.Monad.Error.Class (throwError)
import Data.Text (Text)
import Data.Vector qualified as Vec
import Servant (Capture, Get, Header, JSON, (:>))
import Servant.Server (err404)

type ListUser =
  "users"
    :> Header "user-agent" Text
    :> Get '[JSON] (Vec.Vector Person)

handleGetUsers :: ServerM ListUser
handleGetUsers _ = do
  AppCtx{runDBIO} <- ask
  runDBIO findAll ()