{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.Endpoint (serverM, API) where

import ApiExample.Domain (Person)
import ApiExample.Endpoint.GetUser
import ApiExample.Endpoint.ListUsers
import ApiExample.Framework (ServerM)
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Servant ((:<|>) ((:<|>)))

$(deriveJSON defaultOptions ''Person)

type API = ListUser :<|> GetUser

serverM :: ServerM API
serverM = handleGetUsers :<|> handleGetUser