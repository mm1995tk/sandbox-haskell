{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Examples.DomainCopied where

import Control.Comonad (($>))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Control.Lens
import MyLib.Validator (Validated, Validation (..), accError, runValidation)

data Name = Name {first :: Text, family :: Text} deriving (Show, Eq)

isValidFirstName :: Validation (Validated (NonEmpty MeErr)) Name
isValidFirstName = Validation $ \name@Name{..} ->
  if Text.null first
    then accError EmptyName
    else pure name

isValidFamilyName :: Validation (Validated (NonEmpty MeErr)) Name
isValidFamilyName = Validation $ \name@Name{..} ->
  if Text.null family
    then accError EmptyName
    else pure name

data Me = Me
  { name :: Name
  , age :: Int
  }
  deriving (Show, Eq)

data MeErr
  = EmptyName
  | Baby
  | TooOld
  deriving (Show)

type MeValidation = Validation (Validated (NonEmpty MeErr)) Me

data MeBuilder = MeBuilder
  { _firstName :: Text
  , _familyName :: Text
  , _age :: Int
  }
  deriving (Show, Generic)
makeLenses ''MeBuilder

toBuilder :: Me -> MeBuilder
toBuilder (Me{name = Name{..}, ..}) = MeBuilder{_familyName = family, _firstName = first, _age = age}

isNameValid :: MeValidation
isNameValid = Validation \me@Me{name} ->
  runValidation (isValidFirstName <> isValidFamilyName) name $> me

isAgeValid :: MeValidation
isAgeValid = Validation \me@Me{..} ->
  if age > 3
    then pure me
    else accError Baby

isAgeOver :: MeValidation
isAgeOver = Validation \me@Me{..} ->
  if age < 30
    then pure me
    else accError TooOld

meValidation :: MeValidation
meValidation = isNameValid <> isAgeValid <> isAgeOver

buildMe :: MeBuilder -> Validated (NonEmpty MeErr) Me
buildMe (MeBuilder{..}) = runValidation meValidation Me{name = Name{family = _familyName, first = _firstName}, age = _age}

grow :: MeBuilder -> MeBuilder
grow me@MeBuilder{..} = me & age .~ _age + 1

exec :: IO ()
exec = do
  let meBuilder = toBuilder Me{name = Name{first = "", family = ""}, age = 0}
      updated =
        meBuilder
          & (firstName .~ "tak")
          & (familyName .~ "mm")
          & (age .~ 28)

  print $ buildMe (grow updated)
