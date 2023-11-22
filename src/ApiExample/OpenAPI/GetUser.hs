{-# LANGUAGE OverloadedRecordDot #-}

module ApiExample.OpenAPI.GetUser where

import ApiExample.Domain (Person)
import ApiExample.Framework
import ApiExample.Infrastructure (findMany'')
import Control.Lens
import Data.OpenApi (HasDescription (description), OpenApi)
import Data.Text (Text)
import Data.Vector qualified as Vec
import Effectful (liftIO)
import Effectful.Error.Dynamic
import MyLib.Utils (infoSubApi)
import Servant hiding (IsSubAPI, throwError)
import Servant.OpenApi.Internal.TypeLevel.API (IsSubAPI)

type Endpoint =
  "users"
    :> CookieAuth
    :> Header "user-agent" Text
    :> Capture "user-id" Text
    :> WithVault Get '[JSON] Person

openapiEndpointInfo :: forall api. (IsSubAPI Endpoint api) => Proxy api -> (OpenApi -> OpenApi)
openapiEndpointInfo = infoSubApi @Endpoint @api Proxy $ description' . sec
 where
  description' = description ?~ "find user"
  sec = securityRequirements [[(Cookie, [])]]

handler :: ServerM Endpoint
handler s _ uid v = do
  liftIO $ print s.userName
  users <- runReaderReqScopeCtx' v . runDBIO $ findMany'' [uid]
  if Vec.null users
    then throwError err404{errBody = "the user you specified is not found"}
    else return (Vec.head users)
