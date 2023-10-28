module ApiExample.Framework.ReqScopeCtx where

import Data.Time.Clock.POSIX (POSIXTime)
import Data.ULID (ULID)
import Data.Vault.Lazy qualified as Vault
import Prelude hiding (lookup)

newVaultKey :: IO (Vault.Key ReqScopeCtx)
newVaultKey = Vault.newKey

data ReqScopeCtx = ReqScopeCtx
  { accessId :: ULID
  , reqAt :: POSIXTime
  }
  deriving (Show)
