module ApiExample.Schema where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types
import Data.OpenApi
import Data.Text qualified as T
import GHC.Generics
import Servant (FromHttpApiData, parseQueryParam)

newtype FullName = FullName T.Text deriving (Generic)

instance ToJSON FullName where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FullName where
  parseJSON t@(String str)
    | T.length str > 3 = pure (FullName str)
    | otherwise = typeMismatch "full-name must be more than 3 character." t
  parseJSON t' = unexpected t'

data PersonRequest = PersonRequest
  { fullName :: FullName
  , age :: Int
  }
  deriving (Generic)

instance ToParamSchema FullName
instance ToSchema FullName
instance ToSchema PersonRequest

$(deriveJSON defaultOptions ''PersonRequest)

data OrderBy = Asc | Desc

instance ToParamSchema OrderBy where
  toParamSchema _ =
    mempty
      & title ?~ "order-by"
      & description ?~ "sort asc or desc"
      & type_ ?~ OpenApiString
      & enum_ ?~ [toJSON Asc, toJSON Desc]

instance ToJSON OrderBy where
  toJSON Asc = toJSON @T.Text "asc"
  toJSON Desc = toJSON @T.Text "desc"

instance FromHttpApiData OrderBy where
  parseQueryParam "asc" = Right Asc
  parseQueryParam "desc" = Right Desc
  parseQueryParam _ = Left "must be \"asc\" or \"desc\""
