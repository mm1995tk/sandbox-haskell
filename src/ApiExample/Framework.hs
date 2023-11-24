module ApiExample.Framework (
  module Types,
  module Sec,
  module Http,
  module ApiExample.Framework,
) where

import ApiExample.Framework.Http as Http
import ApiExample.Framework.Security as Sec
import ApiExample.Framework.Types as Types
import Data.Aeson (Key, ToJSON (..), Value)
import Data.Data (Proxy)
import Data.OpenApi (OpenApi)
import Effectful (Eff, liftIO)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Dynamic
import Hasql.Session qualified as HSession
import Hasql.Transaction qualified as Tx
import Servant (Vault, (:>))
import Servant.OpenApi.TypeLevel (IsSubAPI)

type WithVault method x y = Vault :> method x y

type OpenApiEndpointInfo endpoint api = (IsSubAPI endpoint api) => Proxy api -> (OpenApi -> OpenApi)

runTx :: Eff (RaiseTransaction : EffectStack) a -> HandlerM a
runTx = interpret handler
 where
  handler _ (RaiseTransaction tx) = transaction tx

runDBIO :: HSession.Session a -> HandlerM a
runDBIO s = do
  ReqScopeCtx{_runDBIO} <- ask
  _runDBIO s

transaction :: Tx.Transaction a -> HandlerM a
transaction s = do
  ReqScopeCtx{_tx} <- ask
  _tx s

logM :: LogLevel -> Maybe [(Key, Value)] -> forall a. (Show a, ToJSON a) => a -> HandlerM ()
logM level customProps msg = do
  ReqScopeCtx{loggers} <- ask
  liftIO $ logIO loggers level customProps msg

logIO :: Loggers -> LogLevel -> Logger
logIO Loggers{..} = \case
  Danger -> danger
  Warning -> warn
  Info -> info
