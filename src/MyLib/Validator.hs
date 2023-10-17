{-# LANGUAGE BlockArguments #-}

module MyLib.Validator (
  Validation (..),
  runValidation,
  accError,
  module Validation,
  Validated,
  Success,
  Failure,
) where

import Control.Monad ((>=>))
import Validation hiding (Validation)
import Validation qualified as V

type Validated = V.Validation
type Success = V.Success
type Failure = V.Failure

data Validation m d where
  Validation :: (Eq d) => (d -> m d) -> Validation m d

runValidation :: Validation m d -> d -> m d
runValidation (Validation f) = f

instance Semigroup (Validation (Either e) record) where
  (Validation f) <> (Validation g) = Validation $ f >=> g

instance (Semigroup err) => Semigroup (Validation (V.Validation err) record) where
  (Validation f) <> (Validation g) = Validation $ \v -> compose (f v) (g v)
    where
      compose (V.Success x) (V.Success y) = if x == y then V.Success x else error ""
      compose (V.Failure x) (V.Failure y) = V.Failure (x <> y)
      compose _ a@(V.Failure _) = a
      compose a@(V.Failure _) _ = a

accError :: (Applicative m) => err -> V.Validation (m err) x
accError = V.Failure . pure
