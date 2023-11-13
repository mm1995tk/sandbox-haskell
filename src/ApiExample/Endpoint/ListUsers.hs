module ApiExample.Endpoint.ListUsers where

import ApiExample.Domain (Person)
import ApiExample.Framework
import ApiExample.Infrastructure (findAll)
import Data.OpenApi (ToParamSchema)
import Data.Text (Text)
import Data.Vector qualified as Vec
import GHC.Generics (Generic)
import Servant (FromHttpApiData (parseQueryParam), Get, Header, JSON, QueryParam, Vault, (:>))

type ListUser =
  "users"
    :> Vault
    :> Header "user-agent" Text
    :> QueryParam "order-by" OrderBy
    :> Get '[JSON] (Vec.Vector Person)

handleGetUsers :: ServerM ListUser
handleGetUsers v _ queryParams = do
  let logInfo = logM v Info

  case queryParams of
    Nothing -> logInfo (Just [("custom", "xxx"), ("accessId", "xxx")]) @Text "none"
    Just Asc -> logInfo Nothing @Text "asc"
    Just Desc -> logInfo Nothing @Text "desc"
  runDBIO findAll

data OrderBy = Asc | Desc deriving (Generic)

instance ToParamSchema OrderBy

instance FromHttpApiData OrderBy where
  parseQueryParam "asc" = Right Asc
  parseQueryParam "desc" = Right Desc
  parseQueryParam _ = Left "must be \"asc\" or \"desc\""
