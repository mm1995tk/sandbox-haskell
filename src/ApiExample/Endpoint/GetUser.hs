module ApiExample.Endpoint.GetUser where

import ApiExample.Domain (Person)
import ApiExample.Framework ( CookieAuth, ServerM, Session (..), runDBIOM)
import ApiExample.Infrastructure (findMany'')
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Vector qualified as Vec
import Servant

type GetUser =
  "users"
    :> CookieAuth
    :> Header "user-agent" Text
    :> Capture "user-id" Text
    :> Get '[JSON] Person

handleGetUser :: ServerM GetUser
handleGetUser Session{userName} _ uid = do
  liftIO $ print userName
  users <- runDBIOM $ findMany'' [uid]
  if Vec.null users
    then throwError err404{errBody = "the user you specified is not found"}
    else return (Vec.head users)
