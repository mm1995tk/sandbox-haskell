{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.Framework.Security where

import ApiExample.Config.Key
import ApiExample.Framework.Http (Http401ErrorRespBody)
import Control.Lens
import Data.Bifunctor (first)
import Data.Map qualified as M
import Data.OpenApi
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import GHC.IsList (fromList)
import MyLib.Utils (showText)
import Network.HTTP.Media ((//))
import Network.Wai (Request (requestHeaders))
import Servant
import Servant.OpenApi
import Servant.OpenApi.Internal (markdownCode)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData)

type Cookies = [(Text, Text)]

extractCookies :: Request -> Maybe (M.Map T.Text T.Text)
extractCookies req = M.fromList . parseCookies . decodeUtf8Lenient <$> lookup "cookie" (requestHeaders req)

parseCookies :: T.Text -> Cookies
parseCookies cookieText =
  toTuple . T.splitOn "=" <$> T.splitOn ";" cookieText
 where
  toTuple [key, v] = (key, v)
  toTuple _ = error "Invalid cookie string format"

data SecuritySchemeKey = Cookie

instance Show SecuritySchemeKey where
  show Cookie = "cookie"

securityRequirements :: (HasSecurity t [SecurityRequirement]) => [[(SecuritySchemeKey, [T.Text])]] -> t -> t
securityRequirements xs = security .~ f xs
 where
  f = fmap $ \xs' -> SecurityRequirement $ fromList (first showText <$> xs')

data Session = Session {userName :: Text, email :: Text}

type CookieAuth = AuthProtect "cookie"

instance (HasOpenApi a) => HasOpenApi (CookieAuth :> a) where
  toOpenApi _ = toOpenApi (Proxy @a) & addDefaultResponse401 keyOfSessionId
   where
    addDefaultResponse401 pname = setResponseWith (\old _new -> alter401 old) 401 (return response401)
     where
      sname = markdownCode pname
      alter401 = description %~ (<> (" or " <> sname))
      response401 = mempty & (description .~ "description401") . (content .~ fromList [("application" // "json", mediaTypeObj)])
      mediaTypeObj = mempty & schema ?~ toSchemaRef (Proxy @Http401ErrorRespBody)

type instance AuthServerData CookieAuth = Session

type AppAuthHandler = AuthHandler Request Session