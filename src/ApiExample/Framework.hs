module ApiExample.Framework (
  module Types,
  module Sec,
  module AppContext,
  module Logger,
  module Http,
  WithVault,
) where

import ApiExample.Framework.AppContext as AppContext
import ApiExample.Framework.Http as Http
import ApiExample.Framework.Logger as Logger
import ApiExample.Framework.Security as Sec
import ApiExample.Framework.Server as Types (
  AppAuthHandler,
  AppCtx,
  AppTx,
  CookieAuth,
  HandlerM,
  LogLevel (..),
  Logger,
  Loggers,
  ReqScopeCtx,
  RunDBIO,
  ServerM,
  Session (..),
  raiseTransaction,
  runHandlerM,
  runReaderReqScopeCtx,
  runReaderReqScopeCtx',
  runTx,
 )
import Servant (Vault, (:>))

type WithVault method x y = Vault :> method x y
