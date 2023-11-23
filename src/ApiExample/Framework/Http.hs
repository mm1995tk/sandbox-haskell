module ApiExample.Framework.Http (Http401ErrorRespBody (..)) where

import Control.Lens
import Data.Aeson (ToJSON)
import Data.Data (Proxy (..), Typeable)
import Data.OpenApi
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.IsList (fromList)

newtype Http401ErrorRespBody = Http401ErrorRespBody {message :: Text} deriving (Generic, Typeable, Show)

instance ToJSON Http401ErrorRespBody
instance ToSchema Http401ErrorRespBody where
  declareNamedSchema p = (schema %~ type') <$> genericDeclareNamedSchema defaultSchemaOptions p
   where
    type' = (type_ ?~ OpenApiObject) . (properties .~ fromList [("message", toSchemaRef (Proxy @Text))])