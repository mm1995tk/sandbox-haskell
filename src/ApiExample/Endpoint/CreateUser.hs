module ApiExample.Endpoint.CreateUser where

import ApiExample.Domain (Person (..))
import ApiExample.Framework (AppCtx (..), CookieAuth, ServerM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (..), ask)
import ApiExample.Infrastructure (findMany, multiInsert)
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text, pack)
import Data.ULID (getULID)
import Data.Vector qualified as Vec
import Servant

data PersonRequest = PersonRequest
  { fullName :: Text
  , age :: Int
  }

$(deriveJSON defaultOptions ''PersonRequest)

type CreateUser =
  "users"
    :> CookieAuth
    :> ReqBody '[JSON] PersonRequest
    :> Post '[JSON] Person

handleCreateUser :: ServerM CreateUser
handleCreateUser _ PersonRequest{..} = do
  AppCtx{tx} <- ask
  ulid <- pack . show <$> liftIO getULID
  maybeUser <- tx $ \exec -> do
    users <- exec findMany [ulid]
    case Vec.find (\Person{personId = x} -> x == ulid) users of
      Just _ -> return Nothing
      Nothing -> do
        let user = Person{personId = ulid, ..}
        exec multiInsert [user]
        return $ Just user
  maybe (throwError err404) return maybeUser
