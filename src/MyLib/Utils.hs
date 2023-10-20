{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module MyLib.Utils where

import Control.Concurrent (threadDelay)

threadDelaySec :: Int -> IO ()
threadDelaySec = threadDelay . (* 1000000)

(|>) :: a -> (a -> b) -> b
arg |> fn = fn arg

pipeline :: a -> (a -> b) -> b
pipeline = (|>)
