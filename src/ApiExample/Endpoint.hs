{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.Endpoint (serverM, API) where

import ApiExample.Domain (Person)
import ApiExample.Endpoint.CreateUser
import ApiExample.Endpoint.GetUser
import ApiExample.Endpoint.GraphQL
import ApiExample.Endpoint.ListUsers
import ApiExample.Framework (ServerM, AppCtx)
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Servant

$(deriveJSON defaultOptions ''Person)

type API = ListUser :<|> GetUser :<|> CreateUser :<|> GraphQL

serverM :: AppCtx -> ServerM API
serverM c = handleGetUsers :<|> handleGetUser :<|> handleCreateUser :<|> handleGql c
