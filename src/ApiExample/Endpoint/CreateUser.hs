module ApiExample.Endpoint.CreateUser where

import ApiExample.Domain (Person (..))
import ApiExample.Framework (CookieAuth, ServerM, transaction)
import ApiExample.Infrastructure
import ApiExample.Schema (FullName (FullName), PersonRequest (..))
import Data.Coerce (coerce)
import Data.Text qualified as T
import Data.Vector qualified as Vec
import MyLib.Utils (getULIDM)
import Servant

type CreateUser =
  "users"
    :> CookieAuth
    :> ReqBody '[JSON] PersonRequest
    :> Post '[JSON] Person

handleCreateUser :: ServerM CreateUser
handleCreateUser _ PersonRequest{..} = do
  ulid <- T.pack . show <$> getULIDM
  maybeUser <- transaction $ do
    users <- findMany' [ulid]
    case Vec.find (\Person{personId = x} -> x == ulid) users of
      Just _ -> return Nothing
      Nothing -> do
        let user = Person{personId = ulid, fullName = coerce fullName, age}
        insertUser user
        return $ Just user
  maybe (throwError err404) return maybeUser
