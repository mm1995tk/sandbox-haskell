{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.Endpoint (serverM, API) where

import ApiExample.Endpoints.GetUser as GetUser

import ApiExample.Domain (Person)
import ApiExample.Endpoints.ListUsers as ListUsers
import ApiExample.Framework (ServerM)
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Servant ((:<|>) ((:<|>)))

$(deriveJSON defaultOptions ''Person)

type API = ListUser :<|> GetUser

serverM :: ServerM API
serverM = handleGetUsers :<|> handleGetUser