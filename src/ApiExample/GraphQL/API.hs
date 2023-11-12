{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ApiExample.GraphQL.API (gqlApi) where

import Data.Morpheus (deriveApp, interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types (RootResolver (..), Undefined)

import Data.Text (Text)

importGQLDocument "src/ApiExample/schema.gql"

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query{deity}
    , mutationResolver = undefined
    , subscriptionResolver = undefined
    }
 where
  deity DeityArgs{name, pow} = do
    pure
      Deity
        { name = pure name
        , power = pure (Just pow)
        }

gqlApi = deriveApp rootResolver
