module ApiExample.Endpoint.ListUsers where

import ApiExample.Domain (Person)
import ApiExample.Framework (AppCtx (..), ServerM)
import ApiExample.Infrastructure (findAll)
import Control.Monad.Reader (MonadReader (..), ask)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Data.Vector qualified as Vec
import Servant (FromHttpApiData (parseQueryParam), Get, Header, JSON, QueryParam, (:>))

data OrderBy = Asc | Desc

instance FromHttpApiData OrderBy where
  parseQueryParam "asc" = Right Asc
  parseQueryParam "desc" = Right Desc
  parseQueryParam _ = Left "must be \"asc\" or \"desc\""

type ListUser =
  "users"
    :> Header "user-agent" Text
    :> QueryParam "order-by" OrderBy
    :> Get '[JSON] (Vec.Vector Person)

handleGetUsers :: ServerM ListUser
handleGetUsers _ Nothing = do
  liftIO $ print "none"
  AppCtx{runDBIO} <- ask
  runDBIO findAll
handleGetUsers _ (Just Asc) = do
  liftIO $ print "asc"

  AppCtx{runDBIO} <- ask
  runDBIO findAll
handleGetUsers _ (Just Desc) = do
  liftIO $ print "desc"

  AppCtx{runDBIO} <- ask
  runDBIO findAll
