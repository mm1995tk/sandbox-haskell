{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.OpenAPI (
  serverM,
  API,
  ReadAPI,
  WriteAPI,
  outputDoc,
) where

import ApiExample.Config.Key (keyOfSessionId)
import ApiExample.Domain (Person)
import ApiExample.Framework.Security
import ApiExample.Framework.Types
import ApiExample.OpenAPI.CreateUser qualified as CreateUser
import ApiExample.OpenAPI.GetUser qualified as GetUser
import ApiExample.OpenAPI.ListUsers qualified as ListUser
import Control.Lens
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.OpenApi
import GHC.IsList (fromList)
import MyLib.Utils (showText)
import Servant
import Servant.OpenApi (subOperations, toOpenApi)
import Servant.OpenApi.Internal.Test (encodePretty)

$(deriveJSON defaultOptions ''Person)

type API = ReadAPI :<|> WriteAPI

type ReadAPI = ListUser.Endpoint :<|> GetUser.Endpoint
type WriteAPI = CreateUser.Endpoint

serverM :: ServerM API
serverM = readApi :<|> writeApi

readApi :: ServerM ReadAPI
readApi = ListUser.handler :<|> GetUser.handler

writeApi :: ServerM WriteAPI
writeApi = CreateUser.handler

apiProxy :: Proxy API
apiProxy = Proxy @API

tagForReadApi :: OpenApi -> OpenApi
tagForReadApi = applyTagsFor (subOperations (Proxy @ReadAPI) (Proxy @API)) ["ReadAPI" & description ?~ "read in crud"]

tagForWriteApi :: OpenApi -> OpenApi
tagForWriteApi = applyTagsFor (subOperations (Proxy @WriteAPI) (Proxy @API)) ["WriteAPI" & description ?~ "all except read in crud"]

openapiEndpointInfo :: OpenApi -> OpenApi
openapiEndpointInfo =
  tagForReadApi
    . tagForWriteApi
    . CreateUser.openapiEndpointInfo apiProxy
    . GetUser.openapiEndpointInfo apiProxy
    . ListUser.openapiEndpointInfo apiProxy

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
    [ (showText Cookie, SecurityScheme (SecuritySchemeApiKey (ApiKeyParams keyOfSessionId ApiKeyCookie)) (Just "サンプル"))
    ]
