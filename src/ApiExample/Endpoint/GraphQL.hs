module ApiExample.Endpoint.GraphQL where

import ApiExample.Framework(ServerM)
import ApiExample.GraphQL.API (gqlApi)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Morpheus(runApp)
import Data.Morpheus.Types (GQLRequest, GQLResponse)
import Servant

type GraphQL =
  "gql"
    :> ReqBody '[JSON] GQLRequest
    :> Post '[JSON] GQLResponse

handleGql :: ServerM GraphQL
handleGql body = liftIO $ runApp @GQLRequest @GQLResponse gqlApi body
