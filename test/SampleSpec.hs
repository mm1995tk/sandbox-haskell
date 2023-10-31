module SampleSpec where

import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)

addOne :: (Num a) => a -> a
addOne = (+ 1)

spec_sample :: Spec
spec_sample = do
  describe "test-test" $ do
    prop "test01"
      $ \x -> addOne x `shouldBe` (x + 1 :: Int)
