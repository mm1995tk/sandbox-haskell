module ApiExample.OpenAPI.ListUsers where

import ApiExample.Domain (Person)
import ApiExample.Framework
import ApiExample.Infrastructure (findAll)
import ApiExample.Schema
import Control.Lens
import Data.OpenApi (description)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Vector qualified as Vec
import MyLib.Utils (infoSubApi)
import Servant (Get, Header, JSON, QueryParam, (:>))

type Endpoint =
  "users"
    :> Header "user-agent" Text
    :> QueryParam "order-by" OrderBy
    :> Get '[JSON] (Vec.Vector Person)

openapiEndpointInfo :: forall api. OpenApiEndpointInfo Endpoint api
openapiEndpointInfo = infoSubApi @Endpoint @api Proxy description'
 where
  description' = description ?~ "list user"

handler :: ServerM Endpoint
handler _ queryParams = do
  let logInfo = logM Info

  case queryParams of
    Nothing -> logInfo (Just [("custom", "xxx"), ("accessId", "xxx")]) @Text "none"
    Just Asc -> logInfo Nothing @Text "asc"
    Just Desc -> logInfo Nothing @Text "desc"
  runDBIO findAll
