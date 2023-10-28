module ApiExample.Endpoint.ListUsers where

import ApiExample.Domain (Person)
import ApiExample.Framework (AppCtx (..), ServerM)

import ApiExample.Infrastructure (findAll)
import Control.Monad.Reader (MonadReader (..), ask)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Data.Vault.Lazy qualified as Vault
import Data.Vector qualified as Vec
import MyLib.Utils (threadDelaySec)
import Servant (FromHttpApiData (parseQueryParam), Get, Header, JSON, QueryParam, Vault, (:>))

data OrderBy = Asc | Desc

instance FromHttpApiData OrderBy where
  parseQueryParam "asc" = Right Asc
  parseQueryParam "desc" = Right Desc
  parseQueryParam _ = Left "must be \"asc\" or \"desc\""

type ListUser =
  "users"
    :> Vault
    :> Header "user-agent" Text
    :> QueryParam "order-by" OrderBy
    :> Get '[JSON] (Vec.Vector Person)

handleGetUsers :: Text -> ServerM ListUser
handleGetUsers a v _ Nothing = do
  AppCtx{vaultKey} <- ask
  let f = Vault.lookup vaultKey v
  liftIO $ print f
  liftIO $ print "none"
  liftIO $ print a
  liftIO $ threadDelaySec 3
  AppCtx{runDBIO} <- ask
  runDBIO findAll
handleGetUsers _ _ _ (Just Asc) = do
  liftIO $ print "asc"

  AppCtx{runDBIO} <- ask
  runDBIO findAll
handleGetUsers _ _ _ (Just Desc) = do
  liftIO $ print "desc"

  AppCtx{runDBIO} <- ask
  runDBIO findAll
