{-# LANGUAGE BlockArguments #-}

module ApiExample.Framework.Logger (mkLogger, logM, logIO) where

import ApiExample.Framework.Security
import ApiExample.Framework.Server
import Control.Monad.Reader hiding (ask)
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap, fromList)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.ULID (ULID)
import Effectful.Reader.Dynamic (ask)
import Network.Wai (Request (queryString, rawPathInfo, remoteHost, requestMethod))

mkLogger :: Maybe Session -> ULID -> POSIXTime -> Request -> LogLevel -> Logger
mkLogger s accessId reqAt req loglevel additionalProps' item = BS.putStrLn . encode $ jsonLog <> sessionInfo <> additionalProps
 where
  jsonLog =
    fromList
      [ ("level", toJSON loglevel)
      , ("accessId", toJSON $ show accessId)
      , ("method", method)
      , ("path", path)
      , ("reqAt", toJSON $ posixSecondsToUTCTime reqAt)
      , ("message", toJSON item)
      , ("remoteHost", remoteHostAddr)
      , ("queryParams", queryParams)
      ]

  sessionInfo :: KeyMap Value
  sessionInfo = case s of
    Nothing -> mempty
    Just Session{..} -> fromList [("userName", toJSON userName)]

  additionalProps = maybe mempty fromList additionalProps'
  path = toJSON . decodeUtf8Lenient $ rawPathInfo req
  method = toJSON . decodeUtf8Lenient $ requestMethod req
  remoteHostAddr = toJSON . show $ remoteHost req
  queryParams = toJSON . show $ queryString req

logM :: LogLevel -> Maybe [(Key, Value)] -> forall a. (Show a, ToJSON a) => a -> HandlerWithReqScopeCtx ()
logM level customProps msg = do
  ReqScopeCtx{loggers} <- ask
  liftIO $ logIO loggers level customProps msg

logIO :: Loggers -> LogLevel -> Logger
logIO Loggers{..} = \case
  Danger -> danger
  Warning -> warn
  Info -> info
