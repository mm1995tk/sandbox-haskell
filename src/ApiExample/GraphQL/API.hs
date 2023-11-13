{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module ApiExample.GraphQL.API (gqlApi) where

import Data.Morpheus (App, deriveApp)
import Data.Morpheus.Types (GQLType, ResolverQ, RootResolver (..), Undefined)
import Data.Text (Text)
import GHC.Generics (Generic)

data Query m = Query
  { deity :: DeityArgs -> m Deity
  , deityAll :: m [Deity]
  }
  deriving (Generic, GQLType)

data Deity = Deity
  { name :: Text
  , power :: Maybe Text
  }
  deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name :: Text
  , pow :: Maybe Text
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query{deity, deityAll}
    , mutationResolver = undefined
    , subscriptionResolver = undefined
    }
 where
  deityAll :: ResolverQ e IO [Deity]
  deityAll =
    return
      [ Deity
          { name = "name01"
          , power = Just "pow1"
          }
      , Deity
          { name = "name02"
          , power = Just "pow2"
          }
      ]
  deity :: DeityArgs -> ResolverQ e IO Deity
  deity DeityArgs{name, pow} =
    pure
      Deity
        { name = name
        , power = pow
        }

gqlApi :: App () IO
gqlApi = deriveApp rootResolver
