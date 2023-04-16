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
    -- * Labels
  , LabelKey (..)
  , Label (..)
  , LabelSelector (..)
    -- * Server metadata
  , ServerID (..)
  , Metadata (..)
  , getMetadata
    -- * Queries
    -- ** Actions
  , ActionStatus (..)
  , ActionCommand (..)
  , ActionID (..)
  , Action (..)
  , getActions
  , getAction
    -- ** Datacenters
  , DatacenterID (..)
  , DatacenterServers (..)
  , Datacenter (..)
  , DatacentersWithRecommendation (..)
  , getDatacenters
  , getDatacenter
    -- ** Locations
  , City (..)
  , LocationID (..)
  , Location (..)
  , getLocations
  , getLocation
    -- ** Server types
  , ServerTypeID (..)
    -- * Errors
  , Error (..)
  , CloudException (..)
    -- * Regions
  , Region (..)
    -- * Resources
  , ResourceID (..)
    -- * Generic queries
  , cloudQuery
    -- * JSON Wrappers
  , WithKey (..)
  , WithMeta (..)
    -- * Response metadata
  , ResponseMeta (..)
  , Pagination (..)
    ) where

-- base
import Control.Exception (Exception, throwIO)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy
import Data.String (fromString)
-- ip
import Net.IPv4 (IPv4)
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
-- http-conduit
import Network.HTTP.Simple qualified as HTTP
-- time
import Data.Time (ZonedTime)
-- country
import Country (Country)

-- | A token used to authenticate requests.
--
--   You can obtain one through the [Hetzner Cloud Console](https://console.hetzner.cloud).
newtype Token = Token ByteString

-- | An error returned by Hetzner.
data Error = Error
  { -- | Error code.
    errorCode :: Text
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
newtype ServerID = ServerID Int deriving (Show, FromJSON, ToJSON)

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
  , metadataPublicIPv4 :: IPv4
    -- | Datacenter.
  , metadataDatacenter :: Text
    -- | Network zone.
  , metadataRegion :: Region
    } deriving Show

instance FromJSON Metadata where
  parseJSON = JSON.withObject "Metadata" $ \o -> Metadata
    <$> o .: "hostname"
    <*> o .: "instance-id"
    <*> o .: "public-ipv4"
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
--   The page argument determines which page will be requested.
--   If not provided, it will request the first page.
--   If a page is requested outside the valid range, an empty
--   list will be returned, not a failure.
--
cloudQuery
  :: FromJSON a
  => ByteString -- ^ Method
  -> ByteString -- ^ Path
  -> Token -- ^ Authorization token
  -> Maybe Int -- ^ Page
  -> IO a
cloudQuery method path (Token token) mpage = do
  let req = HTTP.setRequestMethod method
          $ HTTP.setRequestSecure True
          $ HTTP.setRequestHost "api.hetzner.cloud"
          $ HTTP.setRequestPort 443
          $ HTTP.setRequestPath ("/v1" <> path)
          $ HTTP.addRequestHeader "Authorization" ("Bearer " <> token)
          $ maybe id (\page -> HTTP.addToRequestQueryString
                                 [("page", Just $ fromString $ show page)]) mpage
          $ HTTP.defaultRequest
  resp <- HTTP.httpBS req
  let body = HTTP.getResponseBody resp
  case div (HTTP.getResponseStatusCode resp) 100 of
    2 -> case JSON.eitherDecodeStrict body of
           Left err -> throwIO $ JSONError resp err
           Right x -> pure x
    _ -> case JSON.eitherDecodeStrict body of
           Left err -> throwIO $ JSONError resp err
           Right x -> throwIO $ CloudError $ withoutKey @"error" x

-- | Wrap a value with the key of the value within a JSON object.
data WithKey (key :: Symbol) a = WithKey { withoutKey :: a } deriving Show

instance Functor (WithKey key) where
  fmap f (WithKey x) = WithKey (f x)

instance (KnownSymbol key, FromJSON a) => FromJSON (WithKey key a) where
  parseJSON =
    let key = symbolVal (Proxy @key)
    in  JSON.withObject ("WithKey " ++ key) $ \o ->
          WithKey <$> o .: fromString key

-- | A value together with response metadata.
--   The type is annotated with the JSON key of the value.
data WithMeta (key :: Symbol) a = WithMeta
  { -- | Response metadata.
    responseMeta :: ResponseMeta
    -- | The value alone, without the metadata.
  , withoutMeta :: a
    } deriving Show

instance Functor (WithMeta key) where
  fmap f x = x { withoutMeta = f $ withoutMeta x }

instance (KnownSymbol key, FromJSON a) => FromJSON (WithMeta key a) where
  parseJSON =
    let key = symbolVal (Proxy @key)
    in  JSON.withObject ("WithMeta:" ++ key) $ \o ->
          WithMeta <$> o .: "meta" <*> o .: fromString key

-- | Metadata attached to a response.
data ResponseMeta = ResponseMeta
  { pagination :: Pagination
    } deriving Show

instance FromJSON ResponseMeta where
  parseJSON = JSON.withObject "ResponseMeta" $ \o ->
    ResponseMeta <$> o .: "pagination"

---------------------------------------------------------------------------------------------------
-- Actions
---------------------------------------------------------------------------------------------------

-- | Status of an action.
data ActionStatus =
    -- | Action is still running. The 'Int' argument is the
    --   progress percentage.
    ActionRunning Int
    -- | Action finished successfully. The finishing time is
    --   provided.
  | ActionSuccess ZonedTime
    -- | Action finished with an error. The finishing time is
    --   provided, together with the error message.
  | ActionError ZonedTime Error
    deriving Show

-- | Command performed by an action.
data ActionCommand =
    CreateServer
  | DeleteServer
  | StartServer
  | StopServer
  | SetFirewallRules
  | ApplyFirewall
  | CreateVolume
  | AttachVolume
    deriving Show

instance FromJSON ActionCommand where
  parseJSON = JSON.withText "ActionCommand" $ \t -> case t of
    "create_server" -> pure CreateServer
    "delete_server" -> pure DeleteServer
    "start_server" -> pure StartServer
    "stop_server" -> pure StopServer
    "set_firewall_rules" -> pure SetFirewallRules
    "apply_firewall" -> pure ApplyFirewall
    "create_volume" -> pure CreateVolume
    "attach_volume" -> pure AttachVolume
    _ -> fail $ "Unknown action command " ++ Text.unpack t

-- | Action identifier.
newtype ActionID = ActionID Int deriving (Eq, Ord, Show, FromJSON)

-- | A resource ID is an ID from one of the available resources.
data ResourceID =
    -- | Server ID.
    ResourceServerID ServerID
    deriving Show

instance FromJSON ResourceID where
  parseJSON = JSON.withObject "ResourceID" $ \o -> do
    t <- o .: "type"
    case t :: Text of
      "server" -> ResourceServerID <$> o .: "id"
      _ -> fail $ "Unknown resource type: " ++ Text.unpack t

-- | Action.
data Action = Action
  { actionID :: ActionID
  , actionCommand :: ActionCommand
  , actionStatus :: ActionStatus
  , actionStarted :: ZonedTime
    -- | Resources the action relates to.
  , actionResources :: [ResourceID]
    } deriving Show

instance FromJSON Action where
  parseJSON = JSON.withObject "Action" $ \o -> do
    status <- do statusText <- o .: "status"
                 case statusText :: Text of
                   "running" -> ActionRunning <$> o .: "progress"
                   "success" -> ActionSuccess <$> o .: "finished"
                   "error" -> ActionError <$> o .: "finished" <*> o .: "error"
                   _ -> fail $ "Unknown action status: " ++ Text.unpack statusText
    Action
     <$> o .: "id"
     <*> o .: "command"
     <*> pure status
     <*> o .: "started"
     <*> o .: "resources"

-- | Get actions.
getActions :: Token -> Maybe Int -> IO (WithMeta "actions" [Action])
getActions = cloudQuery "GET" "/actions"

-- | Get a single action.
getAction :: Token -> ActionID -> IO Action
getAction token (ActionID i) = withoutKey @"action" <$>
  cloudQuery "GET" ("/actions/" <> fromString (show i)) token Nothing

---------------------------------------------------------------------------------------------------
-- Datacenters
---------------------------------------------------------------------------------------------------

-- | Datacenter identifier.
newtype DatacenterID = DatacenterID Int deriving (Eq, Ord, Show, FromJSON)

data DatacenterServers = DatacenterServers
  { availableServers :: [ServerTypeID]
  , migrationAvailableServers :: [ServerTypeID]
  , supportedServers :: [ServerTypeID]
    } deriving Show

instance FromJSON DatacenterServers where
  parseJSON = JSON.withObject "DatacenterServers" $ \o -> DatacenterServers
    <$> o .: "available"
    <*> o .: "available_for_migration"
    <*> o .: "supported"

data Datacenter = Datacenter
  { datacenterID :: DatacenterID
  , datacenterName :: Text
  , datacenterDescription :: Text
  , datacenterLocation :: Location
  , datacenterServers :: DatacenterServers
    } deriving Show

instance FromJSON Datacenter where
  parseJSON = JSON.withObject "Datacenter" $ \o -> Datacenter
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "description"
    <*> o .: "location"
    <*> o .: "server_types"

data DatacentersWithRecommendation = DatacentersWithRecommendation
  { datacenters :: [Datacenter]
    -- | The datacenter which is recommended to be used to create
    --   new servers.
  , datacenterRecommendation :: DatacenterID
    } deriving Show

instance FromJSON DatacentersWithRecommendation where
  parseJSON = JSON.withObject "DatacentersWithRecommendation" $ \o -> DatacentersWithRecommendation
    <$> o .: "datacenters"
    <*> o .: "recommendation"

-- | Get all datacenters.
getDatacenters :: Token -> IO DatacentersWithRecommendation
getDatacenters token = cloudQuery "GET" "/datacenters" token Nothing

-- | Get a single datacenter.
getDatacenter :: Token -> DatacenterID -> IO Datacenter
getDatacenter token (DatacenterID i) = withoutKey @"datacenter" <$>
  cloudQuery "GET" ("/datacenters/" <> fromString (show i)) token Nothing

---------------------------------------------------------------------------------------------------
-- Locations
---------------------------------------------------------------------------------------------------

data City =
    Falkenstein
  | Nuremberg
  | Helsinki
  | AshburnVA
  | HillsboroOR
    deriving (Eq, Show)

instance FromJSON City where
  parseJSON = JSON.withText "City" $ \t -> case t of
    "Falkenstein" -> pure Falkenstein
    "Nuremberg" -> pure Nuremberg
    "Helsinki" -> pure Helsinki
    "Ashburn, VA" -> pure AshburnVA
    "Hillsboro, OR" -> pure HillsboroOR
    _ -> fail $ "Unknown city: " ++ Text.unpack t

-- | Location identifier.
newtype LocationID = LocationID Int deriving (Eq, Ord, Show, FromJSON)

data Location = Location
  { locationCity :: City
  , locationCountry :: Country
  , locationDescription :: Text
  , locationID :: LocationID
  , locationLatitude :: Double
  , locationLongitude :: Double
  , locationName :: Text
  , locationRegion :: Region
    } deriving Show

instance FromJSON Location where
  parseJSON = JSON.withObject "Location" $ \o -> Location
    <$> o .: "city"
    <*> o .: "country"
    <*> o .: "description"
    <*> o .: "id"
    <*> o .: "latitude"
    <*> o .: "longitude"
    <*> o .: "name"
    <*> o .: "network_zone"

-- | Get all locations.
getLocations :: Token -> IO [Location]
getLocations token = withoutKey @"locations" <$>
  cloudQuery "GET" "/locations" token Nothing

-- | Get a single location.
getLocation :: Token -> LocationID -> IO Location
getLocation token (LocationID i) = withoutKey @"location" <$>
  cloudQuery "GET" ("/locations/" <> fromString (show i)) token Nothing

---------------------------------------------------------------------------------------------------
-- Server Types
---------------------------------------------------------------------------------------------------

-- | Server type identifier.
newtype ServerTypeID = ServerTypeID Int deriving (Eq, Ord, Show, FromJSON)
