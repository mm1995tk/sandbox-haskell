module ApiExample.Infrastructure.Model (Person(..)) where

import Data.Int (Int32)
import Data.Text (Text)

data Person = Person
  { personId :: Text
  , age :: Int32
  , fullName :: Text
  }
  deriving (Show)