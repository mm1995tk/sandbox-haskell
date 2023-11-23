module ApiExample.Framework (
  module Server,
  module Sec,
  module Http,
  WithVault,
) where

import ApiExample.Framework.Http as Http
import ApiExample.Framework.Security as Sec
import ApiExample.Framework.Server as Server
import Servant (Vault, (:>))

type WithVault method x y = Vault :> method x y
