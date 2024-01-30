module Decimal where

import Data.Decimal

exec :: IO ()
exec = do
  print @Decimal $ fromRational x
  return ()

x :: Rational
x = 50 / 15
