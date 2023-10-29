{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module MyLib.Utils where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Either (fromRight)
import Data.Text qualified as T
import Data.ULID (ULID, getULID, ulidFromInteger)
import Data.ULID.Base32 (decode)

threadDelaySec :: Int -> IO ()
threadDelaySec = threadDelay . (* 1000000)

(|>) :: a -> (a -> b) -> b
arg |> fn = fn arg

pipeline :: a -> (a -> b) -> b
pipeline = (|>)

ulidFromText :: T.Text -> ULID
ulidFromText t = case decode 26 t of
  [(i, _)] -> fromRight (error "") $ ulidFromInteger i
  _ -> error ""

ulidToText :: ULID -> T.Text
ulidToText = T.pack . show

getULIDM :: (MonadIO m) => m ULID
getULIDM = liftIO getULID