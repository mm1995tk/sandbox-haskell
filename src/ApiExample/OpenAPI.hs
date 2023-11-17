module ApiExample.OpenAPI where

import ApiExample.Endpoint
import Control.Lens 
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.OpenApi
import GHC.IsList ( fromList)
import Servant
import Servant.OpenApi (subOperations, toOpenApi)
import Servant.OpenApi.Internal.Test

outputDoc :: IO ()
outputDoc =
  BSL8.writeFile "openapi.json" $ encodePretty $ toOpenApi (Proxy @API)
    & info . title .~ "User API"
    & info . version .~ "1.0"
    & info . description ?~ "This is an API for the Users service"
    & info . license ?~ "MIT"
    & servers .~ ["https://example.com"]
    & applyTagsFor (subOperations (Proxy @ReadAPI) (Proxy @API)) ["ReadAPI" & description ?~ "read in crud"]
    & applyTagsFor (subOperations (Proxy @WriteAPI) (Proxy @API)) ["WriteAPI" & description ?~ "all except read in crud"]
    & components . securitySchemes .~ SecurityDefinitions (fromList defs)
 where
  defs =
    [ ("Bearer", s)
    ]

  s = SecurityScheme (SecuritySchemeHttp (HttpSchemeBearer Nothing)) (Just "サンプル")