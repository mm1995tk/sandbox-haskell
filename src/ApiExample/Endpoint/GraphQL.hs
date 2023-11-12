module ApiExample.Endpoint.GraphQL (handleGql, GraphQL) where

import ApiExample.Framework (ServerM)
import ApiExample.GraphQL.API (gqlApi)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy.Char8
import Data.Data (Typeable)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Morpheus (App, httpPlayground, runApp)
import Data.Morpheus.Types (GQLRequest, GQLResponse, render)
import Data.Text (Text)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import GHC.TypeLits
import Network.HTTP.Media ((//), (/:))
import Servant

data HTML deriving (Typeable)

instance Accept HTML where
  contentTypes _ = "text" // "html" /: ("charset", "utf-8") :| ["text" // "html"]

instance MimeRender HTML ByteString where
  mimeRender _ = id

type API = ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse

type Schema = "schema.gql" :> Get '[PlainText] Text

type Playground = Get '[HTML] ByteString

type Endpoint (name :: Symbol) = name :> (API :<|> Schema :<|> Playground)

type GraphQL = Endpoint "gql"

handleGql :: ServerM GraphQL
handleGql = api :<|> withSchema gqlApi :<|> pure httpPlayground
 where
  api body = liftIO $ runApp @GQLRequest @GQLResponse gqlApi body

withSchema :: (Applicative f) => App e m -> f Text
withSchema = pure . LT.toStrict . decodeUtf8 . render