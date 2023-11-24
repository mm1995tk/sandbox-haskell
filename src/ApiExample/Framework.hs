module ApiExample.Framework (
  module Server,
  module Sec,
  module Http,
  WithVault,
  OpenApiEndpointInfo,
) where

import ApiExample.Framework.Http as Http
import ApiExample.Framework.Security as Sec
import ApiExample.Framework.Server as Server
import Data.Data (Proxy)
import Data.OpenApi (OpenApi)
import Servant (Vault, (:>))
import Servant.OpenApi.TypeLevel (IsSubAPI)

type WithVault method x y = Vault :> method x y

type OpenApiEndpointInfo endpoint api = (IsSubAPI endpoint api) => Proxy api -> (OpenApi -> OpenApi)