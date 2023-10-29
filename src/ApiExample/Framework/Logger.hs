{-# LANGUAGE BlockArguments #-}

module ApiExample.Framework.Logger (mkLogger, logM, logIO, getLoggers) where

import ApiExample.Framework.Types
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.KeyMap (fromList)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.ULID (ULID)
import Data.Vault.Lazy qualified as Vault
import Network.Wai (Request (queryString, rawPathInfo, remoteHost, requestMethod))

mkLogger :: ULID -> POSIXTime -> Request -> LogLevel -> Logger
mkLogger accessId reqAt req loglevel additionalProps' item = BS.putStrLn . encode $ jsonLog <> additionalProps
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

  additionalProps = maybe mempty fromList additionalProps'
  path = toJSON . decodeUtf8Lenient $ rawPathInfo req
  method = toJSON . decodeUtf8Lenient $ requestMethod req
  remoteHostAddr = toJSON . show $ remoteHost req
  queryParams = toJSON . show $ queryString req

logM :: Vault.Vault -> LogLevel -> Maybe [(Key, Value)] -> forall a. (Show a, ToJSON a) => a -> HandlerM ()
logM v level customProps msg = do
  loggers <- getLoggers v
  liftIO $ logIO loggers level customProps msg

logIO :: Loggers -> LogLevel -> Logger
logIO Loggers{..} = \case
  Danger -> danger
  Warning -> warn
  Info -> info

getLoggers :: Vault.Vault -> HandlerM Loggers
getLoggers v = asks loggers' <*> pure v
 where
  loggers' AppCtx{reqScopeCtx = f} v' = let ReqScopeCtx{loggers} = f v' in loggers
