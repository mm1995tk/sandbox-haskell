module ApiExample.Endpoint.GraphQL where

import ApiExample.Domain (Person)
import ApiExample.Framework
import ApiExample.GraphQL.API (gqlApi)
import ApiExample.Infrastructure (findMany'')
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Morpheus
import Data.Morpheus.Types (GQLRequest, GQLResponse)
import Data.Text (Text)
import Data.Vector qualified as Vec
import Servant

type GraphQL =
  "gql"
    :> ReqBody '[JSON] GQLRequest
    :> Post '[JSON] GQLResponse

handleGql :: ServerM GraphQL
handleGql body = liftIO $ runApp @GQLRequest @GQLResponse gqlApi body
