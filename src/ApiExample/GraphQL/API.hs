module ApiExample.GraphQL.API (gqlApi) where

import Data.Morpheus (deriveApp, App)
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

gqlApi :: App () IO
gqlApi = deriveApp rootResolver
