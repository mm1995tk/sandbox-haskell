module ApiExample.Schema where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types
import Data.Data (Typeable)
import Data.OpenApi (
  OpenApiType (..),
  ToParamSchema (..),
  ToSchema (..),
  defaultSchemaOptions,
  description,
  enum_,
  example,
  genericDeclareNamedSchema,
  minLength,
  schema,
  title,
  type_,
 )
import Data.Text qualified as T
import GHC.Generics
import Servant (FromHttpApiData, parseQueryParam)

newtype FullName = FullName T.Text deriving (Generic, Typeable)
instance ToJSON FullName where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FullName where
  parseJSON t@(String str)
    | T.length str > 3 = pure (FullName str)
    | otherwise = typeMismatch "full-name must be more than 3 character." t
  parseJSON t' = unexpected t'

instance ToSchema FullName where
  declareNamedSchema p = (schema %~ example' . minLength') <$> genericDeclareNamedSchema defaultSchemaOptions p
   where
    example' = example ?~ toJSON (FullName "namenamename")
    minLength' = minLength ?~ 4

data PersonRequest = PersonRequest
  { fullName :: FullName
  , age :: Int
  }
  deriving (Generic, Typeable)

$(deriveJSON defaultOptions ''PersonRequest)

instance ToSchema PersonRequest where
  declareNamedSchema p = (schema %~ example') <$> genericDeclareNamedSchema defaultSchemaOptions p
   where
    example' = example ?~ toJSON PersonRequest{fullName = FullName "person-name", age = 30}

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
