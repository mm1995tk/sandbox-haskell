module ApiExample.Endpoint.ListUsers where

import ApiExample.Domain (Person)
import ApiExample.Framework
import ApiExample.Framework.Types
import ApiExample.Infrastructure (findAll)
import Data.Text (Text)
import Data.Vector qualified as Vec
import Servant (FromHttpApiData (parseQueryParam), Get, Header, JSON, QueryParam, Vault, (:>))

type ListUser =
  "users"
    :> Header "user-agent" Text
    :> QueryParam "order-by" OrderBy
    :> Vault
    :> Get '[JSON] (Vec.Vector Person)

handleGetUsers :: ServerM ListUser
handleGetUsers _ queryParams = runHandlerX $ do
  let logInfo = logM Info

  case queryParams of
    Nothing -> logInfo (Just [("custom", "xxx"), ("accessId", "xxx")]) @Text "none"
    Just Asc -> logInfo Nothing @Text "asc"
    Just Desc -> logInfo Nothing @Text "desc"
  runDBIO findAll

data OrderBy = Asc | Desc

instance FromHttpApiData OrderBy where
  parseQueryParam "asc" = Right Asc
  parseQueryParam "desc" = Right Desc
  parseQueryParam _ = Left "must be \"asc\" or \"desc\""
