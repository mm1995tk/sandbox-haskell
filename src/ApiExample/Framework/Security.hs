module ApiExample.Framework.Security (extractCookies) where

import ApiExample.Framework.Types (Cookies)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Network.Wai (Request (requestHeaders))

extractCookies :: Request -> Maybe (M.Map T.Text T.Text)
extractCookies req = M.fromList . parseCookies . decodeUtf8Lenient <$> lookup "cookie" (requestHeaders req)

parseCookies :: T.Text -> Cookies
parseCookies cookieText =
  toTuple . T.splitOn "=" <$> T.splitOn ";" cookieText
 where
  toTuple [key, value] = (key, value)
  toTuple _ = error "Invalid cookie string format"