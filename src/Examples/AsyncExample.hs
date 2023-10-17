{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Examples.AsyncExample where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Data.Functor (($>))
import Data.Text (Text)

exec :: IO ()
exec = do
  print @Int 0

k :: IO Int
k = do
  withAsync (heavyCalc 5) \promise -> do
    print @Int 777
    r <- wait promise
    return (r + 1)

d :: IO Int
d = withAsync (return (1 :: Int)) $ \a -> do
  withAsync (return (2 :: Int)) \b -> do
    aa <- wait a
    bb <- wait b
    return $ aa + bb * 2

e :: IO (Either Int Int)
e = race (threadDelay 1111 $> 3) (threadDelay 1001 $> 9)

heavyCalc :: Num b => b -> IO b
heavyCalc n = do
  print @Text "9999"
  threadDelay 1000000
  return $ n + 1