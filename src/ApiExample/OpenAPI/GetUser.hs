{-# LANGUAGE OverloadedRecordDot #-}

module ApiExample.OpenAPI.GetUser where

import ApiExample.Domain (Person)
import ApiExample.Framework
import ApiExample.Infrastructure (findMany'')
import Control.Lens
import Data.OpenApi (HasDescription (description))
import Data.Text (Text)
import Data.Vector qualified as Vec
import Effectful (liftIO)
import Effectful.Error.Dynamic
import MyLib.Utils (infoSubApi)
import Servant hiding (IsSubAPI, throwError)

type Endpoint =
  "users"
    :> CookieAuth
    :> Header "user-agent" Text
    :> Capture "user-id" Text
    :> Get '[JSON] Person

openapiEndpointInfo :: forall api. OpenApiEndpointInfo Endpoint api
openapiEndpointInfo = infoSubApi @Endpoint @api Proxy $ description' . sec
 where
  description' = description ?~ "find user"
  sec = securityRequirements [[(Cookie, [])]]

handler :: ServerM Endpoint
handler s _ uid = do
  liftIO $ print s.userName
  users <- runDBIO $ findMany'' [uid]
  if Vec.null users
    then throwError err404{errBody = "the user you specified is not found"}
    else return (Vec.head users)
