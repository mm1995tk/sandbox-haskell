{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module ApiExample.GraphQL.API (gqlApi) where

import Data.Morpheus (App, deriveApp)
import Data.Morpheus.Types (GQLType, QUERY, Resolver, ResolverQ, RootResolver (..), Undefined)
import Data.Text (Text)
import GHC.Generics (Generic)

data Query m = Query
  { deity :: DeityArgs -> m (Deity m)
  , deityAll :: m [Deity m]
  }
  deriving (Generic, GQLType)

data Child = Child
  { name :: Text
  , age :: Int
  }
  deriving (Generic, GQLType)

data Deity m = Deity
  { name :: Text
  , power :: Maybe Text
  , children :: m [Child]
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
  deityAll :: ResolverQ e IO [Deity (Resolver QUERY e IO)]
  deityAll =
    return
      [ Deity
          { name = "name01"
          , power = Just "pow1"
          , children = children 0
          }
      , Deity
          { name = "name02"
          , power = Just "pow2"
          , children = children 1
          }
      ]
  deity :: DeityArgs -> ResolverQ e IO (Deity (Resolver QUERY e IO))
  deity DeityArgs{name, pow} =
    return
      Deity
        { name = name
        , power = pow
        , children = children 2
        }

  children :: Int -> ResolverQ e IO [Child]
  children age = pure [Child{name = "x", age}]

gqlApi :: App () IO
gqlApi = deriveApp rootResolver
