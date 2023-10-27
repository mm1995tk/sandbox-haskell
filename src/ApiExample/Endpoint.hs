{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.Endpoint (serverM, API) where

import ApiExample.Domain (Person)
import ApiExample.Endpoint.CreateUser
import ApiExample.Endpoint.GetUser
import ApiExample.Endpoint.ListUsers
import ApiExample.Framework (ServerM)
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import Servant (Header, (:<|>) ((:<|>)), (:>))

$(deriveJSON defaultOptions ''Person)

type API = Header "x-custom-accessId" Text :> (ListUser :<|> GetUser :<|> CreateUser)

serverM :: ServerM API
serverM (Just accessId) = handleGetUsers accessId :<|> handleGetUser :<|> handleCreateUser
serverM Nothing = error ""