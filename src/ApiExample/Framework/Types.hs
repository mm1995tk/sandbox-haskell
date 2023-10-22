module ApiExample.Framework.Types where

import Control.Monad.Reader (ReaderT)
import Data.Text
import Data.Time.Clock.POSIX (POSIXTime)
import Data.ULID (ULID)
import Hasql.Statement qualified as Hstmt
import Hasql.Transaction qualified as Tx
import Network.Wai (Request)
import Servant (AuthProtect, Handler)
import Servant.Server (HasServer (ServerT))
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData)

type HandlerM = ReaderT AppCtx Handler

type RunDBIO = forall param result. Hstmt.Statement param result -> param -> HandlerM result

type AppTx = forall a. ((forall param result. Hstmt.Statement param result -> param -> Tx.Transaction result) -> Tx.Transaction a) -> HandlerM a

data AppCtx = AppCtx
  { runDBIO :: RunDBIO
  , tx :: AppTx
  , accessId :: ULID
  , reqAt :: POSIXTime
  }

type ServerM api = ServerT api HandlerM

type Cookies = [(Text, Text)]

data Session = Session {userName :: Text, email :: Text}

type CookieAuth = AuthProtect "cookie"

type instance AuthServerData CookieAuth = Session

type AppAuthHandler = AuthHandler Request Session
