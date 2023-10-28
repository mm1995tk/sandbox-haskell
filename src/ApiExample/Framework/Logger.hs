module ApiExample.Framework.Logger where

import ApiExample.Framework.Types
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Time.Clock.POSIX (POSIXTime)
import Data.ULID (ULID)

mkLogger :: ULID -> POSIXTime -> LogLevel -> Logger
mkLogger accessId reqAt loglevel item = BS.putStrLn jsonLog
 where
  jsonLog :: BS.ByteString
  jsonLog =
    encode
      $ object
        [ "accessId" .= show accessId
        , "reqAt" .= reqAt
        , "level" .= show loglevel
        , "message" .= item
        ]