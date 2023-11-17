module ApiExample.Schema where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types
import Data.Text qualified as T
import GHC.Generics
import Data.OpenApi (ToSchema, ToParamSchema)

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
