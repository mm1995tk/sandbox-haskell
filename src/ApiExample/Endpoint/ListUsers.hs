module ApiExample.Endpoint.ListUsers where

import ApiExample.Domain (Person)
import ApiExample.Framework

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
  let logInfo = logM v Info
  logInfo (Just [("custom", "xxx"), ("accessId", "xxx")]) @Text "none-0"

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
