{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Examples.DomainEither where

import GHC.Generics (Generic)
import Control.Lens
import MyLib.Validator (Validation (..), runValidation)

data Me = Me
  { name :: String
  , age :: Int
  }
  deriving (Show, Eq)

data MeErr
  = EmptyName
  | Baby
  | TooOld
  deriving (Show)

type MeValidation = Validation (Either MeErr) Me

data MeBuilder = MeBuilder
  { _name :: String
  , _age :: Int
  }
  deriving (Show, Generic)
makeLenses ''MeBuilder

toBuilder :: Me -> MeBuilder
toBuilder (Me{..}) = MeBuilder{_name = name, _age = age}

isNameValid :: MeValidation
isNameValid = Validation validate
  where
    validate me@Me{..} =
      if not . null $ name
        then Right me
        else Left EmptyName

isAgeValid :: MeValidation
isAgeValid = Validation \me@Me{..} ->
  if age > 3
    then Right me
    else Left Baby

isAgeOver :: MeValidation
isAgeOver = Validation \me@Me{..} ->
  if age < 30
    then Right me
    else Left TooOld

meValidation :: MeValidation
meValidation = isNameValid <> isAgeValid <> isAgeOver

buildMe :: MeBuilder -> Either MeErr Me
buildMe (MeBuilder{..}) = runValidation meValidation Me{name = _name, age = _age}

grow :: MeBuilder -> MeBuilder
grow me@MeBuilder{..} = me & age .~ _age + 1

exec :: IO ()
exec = do
  let meBuilder = toBuilder Me{name = "", age = 0}
      updated =
        meBuilder
          & (name .~ "mm")
          . (age .~ 28)

  print $ buildMe (grow updated)
