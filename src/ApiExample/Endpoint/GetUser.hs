module ApiExample.Endpoint.GetUser where

import ApiExample.Domain (Person)
import ApiExample.Framework
import ApiExample.Infrastructure (findMany'')
import Control.Lens
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.OpenApi (HasDescription (description), OpenApi)
import Data.Text (Text)
import Data.Vector qualified as Vec
import MyLib.Utils (infoSubApi)
import Servant hiding (IsSubAPI)
import Servant.OpenApi.Internal.TypeLevel.API (IsSubAPI)

type Endpoint =
  "users"
    :> CookieAuth
    :> Header "user-agent" Text
    :> Capture "user-id" Text
    :> Get '[JSON] Person

openapiEndpointInfo :: forall api. (IsSubAPI Endpoint api) => Proxy api -> (OpenApi -> OpenApi)
openapiEndpointInfo = infoSubApi @Endpoint @api Proxy $ description' . sec
 where
  description' = description ?~ "find user"
  sec = securityRequirements [[(Bearer, [])]]

handler :: ServerM Endpoint
handler Session{userName} _ uid = do
  liftIO $ print userName
  users <- runDBIO $ findMany'' [uid]
  if Vec.null users
    then throwError err404{errBody = "the user you specified is not found"}
    else return (Vec.head users)
