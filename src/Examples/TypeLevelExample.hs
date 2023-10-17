module Examples.TypeLevelExample (exec) where

import Data.Data
import Data.Text

data Showable = forall x. (Show x) => Showable x

instance Show Showable where
  show :: Showable -> String
  show (Showable x) = show x

heteroList :: [Showable]
heteroList =
  [ Showable @Int 1
  , Showable @(String, String) ("qqq", "")
  , Showable @(Int, Text) (1, "ppap")
  ]

toString3 :: forall a. (Typeable a) => a -> Either String String
toString3 x = case eqT @a @String of
  Just Refl -> Right x
  Nothing -> Left "it's not string"

data Poi = Poi deriving (Typeable, Show)

exec :: IO ()
exec = do
  print $ foo Prelude.length
  print $ toString3 @Text "llll"
  mapM_ print heteroList
  print $ typeRep (Proxy @Poi)

-- type family MyType a where
--   MyType Int = String
--   MyType Char = Bool
--   MyType a = a

-- forall取るとエラーになる。rankNType
foo :: (forall a. [a] -> Int) -> Int
foo f = f @Int [1, 2, 3] + f "string"

-- data PeanoNat
--   = Zero
--   | Succ PeanoNat

-- data SizedList (n :: PeanoNat) a where
--   Nil :: SizedList 'Zero a
--   Cons :: a -> SizedList n a -> SizedList ('Succ n) a

-- x = add . add $ add (Nil @Int)
--   where
--     add = Cons 1

-- remove :: SizedList (Succ (n :: PeanoNat)) a -> SizedList n a
-- remove (Cons _ v) = v

-- data ABC = A | B | C

-- data Mask (x :: ABC) where
--   J :: String -> Mask A
--   K :: Mask B

-- x = J ""

-- x :: Proxy "pppppp"
-- x = Proxy @"pppppp"