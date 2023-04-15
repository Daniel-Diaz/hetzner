---------------------------------------------------------------------------------------------------

-- | Hetzner Cloud API client.
--
--   More information can be found on the
--   [official documentation](https://docs.hetzner.cloud).
--
--   Although not necessary, this module was designed with
--   qualified imports in mind. For example:
--
-- > import qualified Hetzner.Cloud as Hetzner
--
module Hetzner.Cloud
  ( -- * Token
    Token (..)
    -- * Errors
  , ErrorCode (..)
  , Error (..)
  , CloudException (..)
    -- * Labels
  , LabelKey (..)
  , Label (..)
  , LabelSelector (..)
    -- * Regions
  , Region (..)
    -- * Pagination
  , Pagination (..)
    -- * Server metadata
  , ServerID
  , Metadata (..)
  , getMetadata
    -- * Actions
  , ActionStatus (..)
    -- * Generic query
  , cloudQuery
  , WithKey (..)
  , WithMeta (..)
    ) where

-- Internal imports
import Hetzner.Cloud.Internal (parseIP)

-- base
import Data.Foldable (find)
import Data.Char (isUpper, toLower)
import Control.Exception (Exception, throwIO)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy
import Data.String (fromString)
-- network
import Network.Socket (HostAddress)
-- bytestring
import Data.ByteString (ByteString)
-- text
import Data.Text (Text)
import Data.Text qualified as Text
-- aeson
import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import Data.Aeson qualified as JSON
-- yaml
import Data.Yaml qualified as Yaml
-- http
import Network.HTTP.Simple qualified as HTTP

-- | A token used to authenticate requests.
--
--   You can obtain one through the [Hetzner Cloud Console](https://console.hetzner.cloud).
newtype Token = Token ByteString

-- | Error codes that can be received when a request returns an error.
--
--   The constructor names follow the original text codes but have been
--   transformed to use camel case.
data ErrorCode =
    -- | Insufficient permissions for this request.
    Forbidden
    -- | Error while parsing or processing the input.
  | InvalidInput
    -- | Invalid JSON input in your request.
  | JsonError
    -- | The item you are trying to access is locked (there is already an Action running).
  | Locked
    -- | Entity not found.
  | NotFound
    -- | Error when sending too many requests.
  | RateLimitExceeded
    -- | Error when exceeding the maximum quantity of a resource for an account.
  | ResourceLimitExceeded
    -- | The requested resource is currently unavailable.
  | ResourceUnavailable
    -- | Error within a service.
  | ServiceError
    -- | One or more of the objects fields must be unique.
  | UniquenessError
    -- | The Action you are trying to start is protected for this resource.
  | Protected
    -- | Cannot perform operation due to maintenance.
  | Maintenance
    -- | The resource has changed during the request, please retry.
  | Conflict
    -- | The corresponding resource does not support the Action.
  | UnsupportedError
    -- | The token is only allowed to perform GET requests.
  | TokenReadonly
    -- | A service or product is currently not available.
  | Unavailable
    deriving (Eq, Show, Enum)

instance FromJSON ErrorCode where
  parseJSON = JSON.withText "ErrorCode" $ \t ->
    let c = Text.concat $ fmap Text.toTitle $ Text.split ((==) '_') t
    in  case find ((==) c . Text.pack . show) [Forbidden ..] of
          Just ec -> pure ec
          _ -> fail $ "Unknown error code: " ++ Text.unpack c

instance ToJSON ErrorCode where
  toJSON = JSON.toJSON . Text.tail . Text.concatMap f . Text.pack . show
    where
      f :: Char -> Text
      f c = if isUpper c then Text.pack ['_', toLower c] else Text.singleton c

-- | An error returned by Hetzner.
data Error = Error
  { -- | Error code.
    errorCode :: ErrorCode
    -- | Error message.
  , errorMessage :: Text
    } deriving Show

instance FromJSON Error where
  parseJSON = JSON.withObject "Error" $ \o ->
    Error <$> o .: "code" <*> o .: "message"

instance ToJSON Error where
  toJSON err = JSON.object [ "code" .= errorCode err, "message" .= errorMessage err ]

-- | Label key.
data LabelKey = LabelKey
  { -- | Optional prefix. If specified, the prefix must be a DNS subdomain.
    labelKeyPrefix :: Maybe Text
    -- | Key name.
  , labelKeyName :: Text
    }

-- | Labels are key-value pairs that can be attached to all resources.
data Label = Label
  { labelKey :: LabelKey
  , labelValue :: Text
    }

-- | Label selectors can be used to filter results.
data LabelSelector =
    -- | Select when label is equal.
    LabelEqual Label
    -- | Select when label is not equal.
  | LabelNotEqual Label
    -- | Select when key is present.
  | KeyPresent LabelKey
    -- | Select when key is not present.
  | KeyNotPresent LabelKey
    -- | Select when label has one of the values.
  | KeyValueIn LabelKey [Text]
    -- | Select when label has none of the values.
  | KeyValueNotIn LabelKey [Text]
    -- | Select only when all selectors succeed.
  | LabelAll [LabelSelector]

-- | Semigroup under /\"and\"/ operation.
instance Semigroup LabelSelector where
  LabelAll xs <> LabelAll ys = LabelAll (xs ++ ys)
  LabelAll xs <> s =
    case xs of
      [] -> s
      _  -> LabelAll (xs ++ [s])
  s <> LabelAll xs =
    case xs of
      [] -> s
      _  -> LabelAll (s : xs)
  s <> s' = LabelAll [s,s']

-- | Neutral element is a selector that always succeeds.
instance Monoid LabelSelector where
  mempty = LabelAll []

-- | Pagination information.
data Pagination = Pagination
  { currentPage :: Int
  , itemsPerPage :: Int
  , previousPage :: Maybe Int
  , nextPage :: Maybe Int
  , lastPage :: Maybe Int
  , totalEntries :: Maybe Int
    } deriving Show

instance FromJSON Pagination where
  parseJSON = JSON.withObject "Pagination" $ \o -> Pagination
    <$> o .:  "page"
    <*> o .:  "per_page"
    <*> o .:? "previous_page"
    <*> o .:? "next_page"
    <*> o .:? "last_page"
    <*> o .:? "total_entries"

-- | Server identifier.
type ServerID = Int

-- | Network zones.
data Region =
    -- | Nuremberg, Falkenstein, Helsinki.
    EUCentral
    -- | Hillsboro (OR).
  | USWest
    -- | Ashburn (VA).
  | USEast deriving (Eq, Show)

instance FromJSON Region where
  parseJSON = JSON.withText "Region" $ \t -> case t of
    "eu-central" -> pure EUCentral
    "us-west" -> pure USWest
    "us-east" -> pure USEast
    _ -> fail $ "Unknown region: " ++ Text.unpack t

instance ToJSON Region where
  toJSON r = case r of
    EUCentral -> "eu-central"
    USWest -> "us-west"
    USEast -> "us-east"

-- | Metadata that any server in the Hetzner cloud can discover
--   about itself.
data Metadata = Metadata
  { -- | Server name.
    metadataName :: Text
    -- | ID of the server.
  , metadataServerID :: ServerID
    -- | Primary public IPv4 address.
  , metadataPublicIPv4 :: HostAddress
    -- | Datacenter.
  , metadataDatacenter :: Text
    -- | Network zone.
  , metadataRegion :: Region
    } deriving Show

instance FromJSON Metadata where
  parseJSON = JSON.withObject "Metadata" $ \o -> do
    iptext <- o .: "public-ipv4"
    case parseIP iptext of
      Left err -> fail $ "Error reading public-ipv4: " ++ err
      Right ip -> Metadata
        <$> o .: "hostname"
        <*> o .: "instance-id"
        <*> pure ip
        <*> o .: "availability-zone"
        <*> o .: "region"

-- | Generic metadata query.
metadataQuery
  :: FromJSON a
  => ByteString -- ^ Path
  -> IO a
metadataQuery path =
  let req = HTTP.setRequestMethod "GET"
          $ HTTP.setRequestSecure False
          $ HTTP.setRequestHost "169.254.169.254"
          $ HTTP.setRequestPort 80
          $ HTTP.setRequestPath ("/hetzner/v1/metadata" <> path)
          $ HTTP.defaultRequest
  in  HTTP.httpBS req >>= Yaml.decodeThrow . HTTP.getResponseBody

-- | Obtain metadata from running server.
getMetadata :: IO Metadata
getMetadata = metadataQuery mempty

-- | Exception produced while performing a query to Hetzner Cloud.
data CloudException =
    CloudError Error
  | JSONError (HTTP.Response ByteString) String
    deriving Show

instance Exception CloudException

-- | Generic Hetzner Cloud query.
--
--   This function is used to implement Hetzner Cloud queries.
--
--   If there is any issue while performing the request, a
--   'CloudException' will be thrown.
--
cloudQuery
  :: FromJSON a
  => ByteString -- ^ Path
  -> ByteString -- ^ Method
  -> Token
  -> IO a
cloudQuery path method (Token token) = do
  let req = HTTP.setRequestMethod method
          $ HTTP.setRequestSecure True
          $ HTTP.setRequestHost "api.hetzner.cloud"
          $ HTTP.setRequestPort 443
          $ HTTP.setRequestPath ("/v1" <> path)
          $ HTTP.addRequestHeader "Authorization" ("Bearer " <> token)
          $ HTTP.defaultRequest
  resp <- HTTP.httpBS req
  let body = HTTP.getResponseBody resp
  case div (HTTP.getResponseStatusCode resp) 100 of
    2 -> case JSON.eitherDecodeStrict body of
           Left err -> throwIO $ JSONError resp err
           Right x -> pure x
    _ -> case JSON.eitherDecodeStrict body of
           Left err -> throwIO $ JSONError resp err
           Right x -> throwIO $ CloudError x

-- | Wrap a value with the key of the value within a JSON object.
data WithKey (key :: Symbol) a = WithKey { withoutKey :: a } deriving Show

instance (KnownSymbol key, FromJSON a) => FromJSON (WithKey key a) where
  parseJSON =
    let key = symbolVal (Proxy :: Proxy key)
    in  JSON.withObject ("WithKey " ++ key) $ \o ->
          WithKey <$> o .: fromString key

-- | A value together with response metadata.
data WithMeta a = WithMeta
  { -- | Response metadata.
    responseMeta :: ResponseMeta
    -- | The value alone, without the metadata.
  , withoutMeta :: a
    } deriving Show

instance Functor WithMeta where
  fmap f x = x { withoutMeta = f $ withoutMeta x }

data ResponseMeta = ResponseMeta
  { pagination :: Pagination
    } deriving Show

---------------------------------------------------------------------------------------------------
-- Actions
---------------------------------------------------------------------------------------------------

-- | Status of an action.
data ActionStatus = ActionRunning | ActionSuccess | ActionError
