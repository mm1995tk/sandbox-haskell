{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module MyLib.Utils where

import Control.Concurrent (threadDelay)
import Control.Lens ((%~), (&))
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Data (Proxy)
import Data.Either (fromRight)
import Data.OpenApi (OpenApi, Operation)
import Data.Text qualified as T
import Data.ULID (ULID, getULID, ulidFromInteger)
import Data.ULID.Base32 (decode)
import Servant.OpenApi (HasOpenApi, subOperations)
import Servant.OpenApi.Internal.TypeLevel (IsSubAPI)

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

infoSubApi :: (IsSubAPI sub api, HasOpenApi sub) => Proxy sub -> (Operation -> Operation) -> Proxy api -> (OpenApi -> OpenApi)
infoSubApi sub operationLens api openapi' = openapi' & subOperations sub api %~ operationLens

showText :: (Show s) => s -> T.Text
showText = T.pack . show
