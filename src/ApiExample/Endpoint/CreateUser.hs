{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.Endpoint.CreateUser where

import ApiExample.Domain (Person (..))
import ApiExample.Framework (CookieAuth, ServerM, transaction)
import ApiExample.Framework.Security
import ApiExample.Infrastructure
import ApiExample.Schema (FullName (FullName), PersonRequest (..))
import Control.Lens
import Data.Coerce (coerce)
import Data.OpenApi
import Data.Text qualified as T
import Data.Vector qualified as Vec
import MyLib.Utils (getULIDM, infoSubApi)
import Servant hiding (AllIsElem, IsIn, IsSubAPI)
import Servant.OpenApi.Internal.TypeLevel.API (IsSubAPI)

type Endpoint =
  "users"
    :> CookieAuth
    :> ReqBody '[JSON] PersonRequest
    :> Post '[JSON] Person

openapiEndpointInfo :: forall api. (IsSubAPI Endpoint api) => Proxy api -> (OpenApi -> OpenApi)
openapiEndpointInfo = infoSubApi @Endpoint @api Proxy $ description' . sec
 where
  description' = description ?~ "create user"
  sec = securityRequirements [[(Bearer, [])]]

handler :: ServerM Endpoint
handler _ PersonRequest{..} = do
  ulid <- T.pack . show <$> getULIDM
  maybeUser <- transaction $ do
    users <- findMany' [ulid]
    case Vec.find (\Person{personId = x} -> x == ulid) users of
      Just _ -> return Nothing
      Nothing -> do
        let user = Person{personId = ulid, fullName = coerce fullName, age}
        insertUser user
        return $ Just user
  maybe (throwError err404) return maybeUser
