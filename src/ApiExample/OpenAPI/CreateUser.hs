{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ApiExample.OpenAPI.CreateUser where

import ApiExample.Domain (Person (..))
import ApiExample.Framework
import ApiExample.Infrastructure
import ApiExample.Schema (FullName (FullName), PersonRequest (..))
import Control.Lens
import Data.Coerce (coerce)
import Data.OpenApi
import Data.Text qualified as T
import Data.Vector qualified as Vec
import Effectful.Error.Dynamic
import MyLib.Utils (getULIDM, infoSubApi)
import Servant hiding (AllIsElem, IsIn, IsSubAPI, throwError)

type Endpoint =
  "users"
    :> CookieAuth
    :> ReqBody '[JSON] PersonRequest
    :> Post '[JSON] Person

openapiEndpointInfo :: forall api. OpenApiEndpointInfo Endpoint api
openapiEndpointInfo = infoSubApi @Endpoint @api Proxy $ description' . sec
 where
  description' = description ?~ "create user"
  sec = securityRequirements [[(Cookie, [])]]

handler :: ServerM Endpoint
handler _ PersonRequest{..} = do
  ulid <- T.pack . show <$> getULIDM
  maybeUser <- transaction $ do
    users <- findMany' [ulid]
    case Vec.find (\p -> p.personId == ulid) users of
      Just _ -> return Nothing
      Nothing -> do
        let user = Person{personId = ulid, fullName = coerce fullName, age}
        insertUser user
        return $ Just user
  maybe (throwError err404) return maybeUser

-- Handler全体をトランザクションで囲む
-- handler :: ServerM Endpoint
-- handler _ PersonRequest{..} = runTx $ do
--   ulid <- T.pack . show <$> getULIDM
--   users <- raiseTransaction $ findMany' [ulid]
--   case Vec.find (\p -> p.personId == ulid) users of
--     Just _ -> throwError err404
--     Nothing -> raiseTransaction $ do
--       let user = Person{personId = ulid, fullName = coerce fullName, age}
--       insertUser user
--       return user
