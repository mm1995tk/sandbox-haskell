module ApiExample.Framework.Security (extractCookies, SecuritySchemeKey (..), securityRequirements) where

import ApiExample.Framework.Types (Cookies)
import Control.Lens
import Data.Bifunctor (first)
import Data.Map qualified as M
import Data.OpenApi (HasSecurity (..), SecurityRequirement (..))
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import GHC.IsList (fromList)
import MyLib.Utils (showText)
import Network.Wai (Request (requestHeaders))

extractCookies :: Request -> Maybe (M.Map T.Text T.Text)
extractCookies req = M.fromList . parseCookies . decodeUtf8Lenient <$> lookup "cookie" (requestHeaders req)

parseCookies :: T.Text -> Cookies
parseCookies cookieText =
  toTuple . T.splitOn "=" <$> T.splitOn ";" cookieText
 where
  toTuple [key, value] = (key, value)
  toTuple _ = error "Invalid cookie string format"

data SecuritySchemeKey = Cookie

instance Show SecuritySchemeKey where
  show Cookie = "cookie"

securityRequirements :: (HasSecurity t [SecurityRequirement]) => [[(SecuritySchemeKey, [T.Text])]] -> t -> t
securityRequirements xs = security .~ f xs
 where
  f = fmap $ \xs' -> SecurityRequirement $ fromList (first showText <$> xs')
