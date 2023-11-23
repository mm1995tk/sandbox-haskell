{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module ApiExample.GraphQL.API (gqlApi, initState) where

import ApiExample.Domain
import ApiExample.Framework.Server (ReqScopeCtx (..))
import ApiExample.Infrastructure (findAll)
import Control.Monad (unless, void)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Hashable (Hashable (..))
import Data.Morpheus (App, deriveApp)
import Data.Morpheus.Types (GQLType, QUERY, Resolver, ResolverQ, RootResolver (..), Undefined)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector (toList)
import GHC.Generics (Generic)
import Haxl.Core

data DatasourceReq a where
  GetDeity :: Text -> DatasourceReq DeityE
  deriving (Typeable)

deriving instance (Eq (DatasourceReq a))

deriving instance Show (DatasourceReq a)
instance ShowP DatasourceReq where showp = show
instance DataSourceName DatasourceReq where
  dataSourceName _ = "UserDataSource"

instance StateKey DatasourceReq where
  data State DatasourceReq = DatasourceReqState

initState :: State DatasourceReq
initState = DatasourceReqState

type Haxl = GenHaxl ReqScopeCtx ()

instance Hashable (DatasourceReq a) where
  hashWithSalt s (GetDeity a) = hashWithSalt s (1 :: Int, a)

instance DataSource ReqScopeCtx DatasourceReq where
  fetch _state _flags appCtx = SyncFetch $ f appCtx

f :: ReqScopeCtx -> [BlockedFetch DatasourceReq] -> IO ()
f ReqScopeCtx{runDBIO'} blockedFetches = unless (null ids) . void $ do
  entities <- runDBIO' findAll
  case entities of
    Left e -> foldl (\acc var -> acc *> putFailure var e) mempty vars
    Right entities' -> mapM_ (\(var, Person{fullName}) -> putSuccess var DeityE{name = fullName}) (zip vars (toList entities'))
 where
  ids :: [Text]
  vars :: [ResultVar DeityE]
  (ids, vars) = unzip [(d, a) | BlockedFetch (GetDeity d) a <- blockedFetches]

getDeity :: (MonadTrans t) => Text -> t Haxl DeityE
getDeity t = lift . dataFetch $ GetDeity t

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

data DeityE = DeityE
  { name :: Text
  }
  deriving (Show)

data DeityArgs = DeityArgs
  { name :: Text
  , pow :: Maybe Text
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver Haxl () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query{deity, deityAll}
    , mutationResolver = undefined
    , subscriptionResolver = undefined
    }

deityAll :: ResolverQ () Haxl [Deity (Resolver QUERY () Haxl)]
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
deity :: DeityArgs -> ResolverQ () Haxl (Deity (Resolver QUERY () Haxl))
deity DeityArgs{name, pow} = do
  DeityE{name = nameE} <- getDeity name
  return
    Deity
      { name = nameE
      , power = pow
      , children = children 2
      }

children :: Int -> ResolverQ () Haxl [Child]
children age = pure [Child{name = "x", age}]

gqlApi :: App () Haxl
gqlApi = deriveApp rootResolver
