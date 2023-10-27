module ApiExample.Endpoint.CreateUser where

import ApiExample.Domain (Person (..))
import ApiExample.Framework (AppCtx (..), CookieAuth, ServerM)
import ApiExample.Infrastructure
import ApiExample.Schema (FullName (FullName), PersonRequest (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (..), ask)
import Data.Coerce (coerce)
import Data.Text qualified as T
import Data.ULID (getULID)
import Data.Vector qualified as Vec
import Servant

type CreateUser =
  "users"
    :> CookieAuth
    :> ReqBody '[JSON] PersonRequest
    :> Post '[JSON] Person

handleCreateUser :: ServerM CreateUser
handleCreateUser _ PersonRequest{..} = do
  AppCtx{tx} <- ask
  ulid <- T.pack . show <$> liftIO getULID
  maybeUser <- tx $ do
    users <- findMany' [ulid]
    case Vec.find (\Person{personId = x} -> x == ulid) users of
      Just _ -> return Nothing
      Nothing -> do
        let user = Person{personId = ulid, fullName = coerce fullName, age}
        insertUser user
        return $ Just user
  maybe (throwError err404) return maybeUser
