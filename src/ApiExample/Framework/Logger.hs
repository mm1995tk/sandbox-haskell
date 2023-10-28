module ApiExample.Framework.Logger where

import ApiExample.Framework.Types
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap, insert)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.ULID (ULID)
import Network.Wai (Request (queryString, rawPathInfo, rawQueryString, remoteHost, requestHeaderHost, requestMethod))

mkLogger :: ULID -> POSIXTime -> Request -> LogLevel -> Logger
mkLogger accessId reqAt req loglevel item = BS.putStrLn jsonLog
 where
  jsonLog =
    encode
      $ object
        [ "level" .= show loglevel
        , "accessId" .= show accessId
        , "method" .= method
        , "path" .= path
        , "reqAt" .= posixSecondsToUTCTime reqAt
        , "message" .= item
        , "remoteHost" .= remoteHostAddr
        , "queryParams" .= queryParams
        ]

  path = decodeUtf8Lenient $ rawPathInfo req
  method = decodeUtf8Lenient $ requestMethod req
  remoteHostAddr = show $ remoteHost req
  queryParams = bimap decodeUtf8Lenient (maybe "" decodeUtf8Lenient) <$> queryString req

v :: Value
v = object []

j :: KeyMap Value
j = undefined

k :: KeyMap Value
k = insert "" v j
