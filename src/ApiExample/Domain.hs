module ApiExample.Domain where

import Data.Text (Text)

data Person = Person
  { personId :: Text
  , age :: Int
  , fullName :: Text
  }
  deriving (Show)
