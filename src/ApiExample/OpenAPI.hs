module ApiExample.OpenAPI where

import ApiExample.Endpoint
import ApiExample.Framework.Security
import ApiExample.Framework.Types
import Control.Lens
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.OpenApi
import Data.Semigroup (sconcat)
import GHC.IsList (fromList)
import MyLib.Utils (showText)
import Servant
import Servant.OpenApi (toOpenApi)
import Servant.OpenApi.Internal.Test (encodePretty)

outputDoc :: IO ()
outputDoc = BSL8.writeFile "openapi.json" $ encodePretty openapi'

openapi' :: OpenApi
openapi' =
  toOpenApi (Proxy @API)
    & info . title .~ "Sandbox API"
    & info . version .~ "1.0"
    & info . description ?~ "This is an API"
    & info . license ?~ "MIT"
    & servers .~ ["https://example.com"]
    & components . schemas %~ (<>) (fromList [("Http401ErrorRespBody", toSchema (Proxy @Http401ErrorRespBody))])
    & components . securitySchemes .~ SecurityDefinitions (fromList defs)
    & openapiEndpointInfo
 where
  defs =
    [ (showText Bearer, SecurityScheme (SecuritySchemeHttp (HttpSchemeBearer Nothing)) (Just "サンプル"))
    ]
