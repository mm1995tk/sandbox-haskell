module ApiExample.Domain where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.OpenApi (ToSchema)

data Person = Person
  { personId :: Text
  , age :: Int
  , fullName :: Text
  }
  deriving (Show, Generic)

instance ToSchema Person
