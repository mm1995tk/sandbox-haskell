{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.Endpoint (
  serverM,
  API,
  ReadAPI,
  WriteAPI,
  openapiEndpointInfo,
) where

import ApiExample.Domain (Person)
import ApiExample.Endpoint.CreateUser qualified as CreateUser
import ApiExample.Endpoint.GetUser
import ApiExample.Endpoint.ListUsers
import ApiExample.Framework (ServerM)
import Control.Lens
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.OpenApi (OpenApi, description)
import Data.OpenApi.Operation (applyTagsFor)
import Servant
import Servant.OpenApi (subOperations)

$(deriveJSON defaultOptions ''Person)

type API = ReadAPI :<|> WriteAPI

type ReadAPI = ListUser :<|> GetUser
type WriteAPI = CreateUser.Endpoint

serverM :: ServerM API
serverM = readApi :<|> writeApi

readApi :: ServerM ReadAPI
readApi = handleGetUsers :<|> handleGetUser

writeApi :: ServerM WriteAPI
writeApi = CreateUser.handler

apiProxy :: Proxy API
apiProxy = Proxy @API

tagForReadApi :: OpenApi -> OpenApi
tagForReadApi = applyTagsFor (subOperations (Proxy @ReadAPI) (Proxy @API)) ["ReadAPI" & description ?~ "read in crud"]

tagForWriteApi :: OpenApi -> OpenApi
tagForWriteApi = applyTagsFor (subOperations (Proxy @WriteAPI) (Proxy @API)) ["WriteAPI" & description ?~ "all except read in crud"]

openapiEndpointInfo :: OpenApi -> OpenApi
openapiEndpointInfo = tagForReadApi . tagForWriteApi . CreateUser.openapiEndpointInfo apiProxy