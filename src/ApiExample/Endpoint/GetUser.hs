module ApiExample.Endpoint.GetUser where

import ApiExample.Domain (Person)
import ApiExample.Framework
import ApiExample.Framework.Types (WithVault, runHandlerX)
import ApiExample.Infrastructure (findMany'')
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Vector qualified as Vec
import Effectful.Error.Dynamic
import Servant hiding (throwError)

type GetUser =
  "users"
    :> CookieAuth
    :> Header "user-agent" Text
    :> Capture "user-id" Text
    :> WithVault Get '[JSON] Person

handleGetUser :: ServerM GetUser
handleGetUser Session{userName} _ uid = runHandlerX $ do
  liftIO $ print userName
  users <- runDBIO $ findMany'' [uid]
  if Vec.null users
    then throwError err404{errBody = "the user you specified is not found"}
    else return (Vec.head users)
