{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module MyLib.Utils where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Control.Exception.Safe (Exception (fromException), Handler (..))
import Control.Lens ((%~))
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Data (Proxy)
import Data.Either (fromRight)
import Data.OpenApi (OpenApi, Operation)
import Data.Text qualified as T
import Data.ULID (ULID, getULID, ulidFromInteger)
import Data.ULID.Base32 (decode)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error, catchError, throwError)
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
infoSubApi sub operationLens api = subOperations sub api %~ operationLens

showText :: (Show s) => s -> T.Text
showText = T.pack . show

catchesEff :: (Foldable t, Error SomeException :> es) => Eff es a -> t (Handler (Eff es) a) -> Eff es a
catchesEff eff handlers = eff `catchError` const handle
 where
  handle e = Prelude.foldr (tryHandler e) (throwError e) handlers
  tryHandler e (Handler handler) res = maybe res handler $ fromException e