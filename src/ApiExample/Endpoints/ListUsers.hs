module ApiExample.Endpoints.ListUsers where

import ApiExample.Domain (Person)
import ApiExample.Framework (AppCtx (..), ServerM)
import Control.Monad.Reader (MonadReader (..), ask)
import ApiExample.Infrastructure (findAll)
import Data.Text (Text)
import Data.Vector qualified as Vec
import Servant (Get, Header, JSON, (:>))

type ListUser =
  "users"
    :> Header "user-agent" Text
    :> Get '[JSON] (Vec.Vector Person)

handleGetUsers :: ServerM ListUser
handleGetUsers _ = do
  AppCtx{runDBIO} <- ask
  runDBIO findAll ()