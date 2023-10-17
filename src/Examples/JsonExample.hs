{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Examples.JsonExample where

import Control.Applicative (empty)
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Name = Name Text deriving (Show, Generic)
makeLenses ''Name

data Validation
  = Validated
  | Invalidated

data Person (x :: Validation) = Person
  { _name :: Name
  , _age :: Int
  }
  deriving (Show, Generic)

makeLenses ''Person

instance ToJSON Name where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON (Person Validated) where
  toEncoding p = pairs (nameEncoded <> ageEncoded)
    where
      nameEncoded = "name" .= (p ^. (name . coerced) :: Text)
      ageEncoded = "age" .= (p ^. age :: Int)

instance FromJSON Name
instance FromJSON (Person Validated) where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = dropUnderscore}
    where
      dropUnderscore "_name" = "name"
      dropUnderscore "_age" = "age"
      dropUnderscore str = str

jsonString :: String
jsonString = "{\"title\": \"The Godfather\",\"year\": 1972,\"starring\": [\"Marlon Brando\",\"Al Pacino\",\"James Caan\"]}"

exec :: IO ()
exec = do
  let p =
        Person
          { _name = Name ""
          , _age = 0
          } ::
          Person Validated
  let rawJson = encode (p & name .~ Name "ppap" & age .~ 28 :: Person Validated)
  print rawJson
  maybe empty print $ decode @(Person Validated) rawJson
  print $ jsonString ^? key "starring" . nth 2 . _String
