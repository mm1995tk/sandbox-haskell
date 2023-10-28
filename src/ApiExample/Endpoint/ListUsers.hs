module ApiExample.Endpoint.ListUsers where

import ApiExample.Domain (Person)
import ApiExample.Framework (AppCtx (..), ServerM, ReqScopeCtx (ReqScopeCtx, loggers), Loggers (Loggers, info))
import ApiExample.Infrastructure (findAll)
import Control.Monad.Reader (MonadReader (..), ask)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Data.Vector qualified as Vec
import MyLib.Utils (threadDelaySec)
import Servant (FromHttpApiData (parseQueryParam), Get, Header, JSON, QueryParam, Vault, (:>))

type ListUser =
  "users"
    :> Vault
    :> Header "user-agent" Text
    :> QueryParam "order-by" OrderBy
    :> Get '[JSON] (Vec.Vector Person)

handleGetUsers :: ServerM ListUser
handleGetUsers v _ Nothing = do
  AppCtx{reqScopeCtx} <- ask
  let ReqScopeCtx{loggers = Loggers{info}} = reqScopeCtx v
  liftIO $ info @Text "none"

  liftIO $ threadDelaySec 3
  AppCtx{runDBIO} <- ask
  runDBIO findAll
handleGetUsers _ _ (Just Asc) = do
  liftIO $ print "asc"

  AppCtx{runDBIO} <- ask
  runDBIO findAll
handleGetUsers _ _ (Just Desc) = do
  liftIO $ print "desc"

  AppCtx{runDBIO} <- ask
  runDBIO findAll

data OrderBy = Asc | Desc

instance FromHttpApiData OrderBy where
  parseQueryParam "asc" = Right Asc
  parseQueryParam "desc" = Right Desc
  parseQueryParam _ = Left "must be \"asc\" or \"desc\""