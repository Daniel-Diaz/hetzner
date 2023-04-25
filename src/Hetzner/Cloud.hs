----------------------------------------------------------------------------------------------------

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
--   == Pagination
--
--   Some requests use pagination. These take a page argument of
--   type @'Maybe' 'Int'@. You can use 'streamPages' to get all pages
--   through a conduit-based stream. For example, to get all servers
--   as a stream:
--
-- > streamPages $ getServers token :: ConduitT i Server m ()
--
--   Or to get all actions as a stream:
--
-- > streamPages $ getActions token :: ConduitT i Action m ()
--
--   If you are not interested in the streaming functionality, you
--   can simply use 'streamToList' to turn the stream into a list:
--
-- > streamToList $ streamPages $ getServers token :: m [Server]
--
--   == Exceptions
--
--   This library makes extensive use of exceptions. Exceptions from
--   this module have type 'CloudException'. All functions that perform
--   requests to Hetzner Cloud can throw this type of exception.
--
module Hetzner.Cloud
  ( -- * Tokens
    Token (..)
    -- * Server metadata
  , Metadata (..)
  , getMetadata
    -- * Hetzner Cloud API

    -- | Sections are in the same order as in the
    --   [official documentation](https://docs.hetzner.cloud).

    -- ** Actions
  , ActionStatus (..)
  , ActionCommand (..)
  , ActionID (..)
  , Action (..)
  , getActions
  , getAction
  , waitForAction
    -- ** Datacenters
  , DatacenterID (..)
  , DatacenterServers (..)
  , Datacenter (..)
  , DatacentersWithRecommendation (..)
  , getDatacenters
  , getDatacenter
    -- ** Firewalls
  , FirewallID (..)
    -- ** Floating IPs
  , FloatingIPID (..)
    -- ** Images
  , OSFlavor (..)
  , ImageType (..)
  , ImageID (..)
  , Image (..)
  , getImages
  , getImage
    -- ** Locations
  , City (..)
  , LocationID (..)
  , Location (..)
  , getLocations
  , getLocation
    -- ** Pricing
  , Price (..)
  , PriceInLocation (..)
    -- ** Servers
  , ServerStatus (..)
  , ServerID (..)
  , Server (..)
  , NewServer (..)
  , defaultNewServer
  , CreatedServer (..)
  , getServers
  , getServer
  , createServer
  , deleteServer
    -- ** Server types
  , Architecture (..)
  , StorageType (..)
  , CPUType (..)
  , ServerTypeID (..)
  , ServerType (..)
  , getServerTypes
    -- ** SSH Keys
  , SSHKeyID (..)
  , SSHKey (..)
  , getSSHKeys
  , getSSHKey
  , createSSHKey
  , deleteSSHKey
  , updateSSHKey
    -- ** Volumes
  , VolumeID (..)
  , VolumeFormat (..)
  , VolumeStatus (..)
  , Volume (..)
  , AttachToServer (..)
  , NewVolume (..)
  , CreatedVolume (..)
  , getVolumes
  , getVolume
  , createVolume
  , deleteVolume
  , updateVolume
    -- * Exceptions
  , Error (..)
  , CloudException (..)
    -- * Labels
  , LabelKey (..)
  , Label (..)
  , LabelMap
  , toLabelMap
  , fromLabelMap
  , LabelSelector (..)
    -- * Other types
    -- ** Regions
  , Region (..)
    -- ** Resources
  , ResourceID (..)
    -- ** Public networks
  , FirewallStatus (..)
  , PublicIPInfo (..)
  , PublicNetwork (..)
    -- * Streaming
  , streamPages
  , streamToList
    -- * Generic interface
    -- ** Generic queries
  , cloudQuery
  , noBody
    -- ** JSON Wrappers
  , WithKey (..)
  , WithMeta (..)
    -- ** Response metadata
  , ResponseMeta (..)
  , Pagination (..)
    ) where

import Hetzner.Cloud.Fingerprint ()
-- base
import Control.Exception (Exception, throwIO)
import Control.Concurrent (threadDelay)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy
import Data.String (fromString)
import GHC.Fingerprint (Fingerprint (..))
import Data.Void
import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (forM_)
import Data.Maybe (isNothing, fromMaybe)
-- ip
import Net.IPv4 (IPv4)
import Net.IPv6 (IPv6, IPv6Range)
-- bytestring
import Data.ByteString (ByteString)
-- text
import Data.Text (Text)
import Data.Text qualified as Text
-- aeson
import Data.Aeson
  ( FromJSON, ToJSON
  , (.:), (.:?), (.=)
  , FromJSONKey, ToJSONKey
    )
import Data.Aeson qualified as JSON
import Data.Aeson.Types qualified as JSON
import Data.Aeson.Key qualified as JSONKey
import Data.Aeson.Encoding qualified as JSONEncoding
-- yaml
import Data.Yaml qualified as Yaml
-- http-conduit
import Network.HTTP.Simple qualified as HTTP
-- time
import Data.Time (ZonedTime)
-- country
import Country (Country)
-- megaparsec
import Text.Megaparsec qualified as Parser
import Text.Megaparsec.Char.Lexer qualified as Parser
-- containers
import Data.Map (Map)
import Data.Map qualified as Map
-- scientific
import Data.Scientific (Scientific)
-- conduit
import Data.Conduit (ConduitT)
import Data.Conduit qualified as Conduit

-- | A token used to authenticate requests. All requests made with a token
--   will have as scope the project where the token was made.
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
  { -- | Optional prefix.
    labelKeyPrefix :: Maybe Text
    -- | Key name.
  , labelKeyName :: Text
    } deriving (Eq, Ord, Show)

type Parser = Parser.Parsec Void Text

labelKeyParser :: Parser LabelKey
labelKeyParser = do
  prefix <- fmap Text.pack $ Parser.some $ Parser.anySingleBut '/'
  atEnd <- Parser.atEnd
  if atEnd
     then pure $ LabelKey Nothing prefix
     else LabelKey (Just prefix) <$> (Parser.single '/' *> Parser.takeRest)

renderKeyParser :: LabelKey -> Text
renderKeyParser k = case labelKeyPrefix k of
  Just prefix -> Text.concat [ prefix, "/", labelKeyName k ]
  _ -> labelKeyName k

instance FromJSON LabelKey where
  parseJSON = JSON.withText "LabelKey" $ \t ->
    either (fail . Parser.errorBundlePretty) pure $
      Parser.runParser labelKeyParser "JSON" t

instance ToJSON LabelKey where
   toJSON = JSON.String . renderKeyParser

instance FromJSONKey LabelKey where
  fromJSONKey = JSON.FromJSONKeyTextParser $ \t ->
    either (fail . Parser.errorBundlePretty) pure $
      Parser.runParser labelKeyParser "JSON key" t

instance ToJSONKey LabelKey where
  toJSONKey =
    JSON.ToJSONKeyText
      (JSONKey.fromText . renderKeyParser)
      (JSONEncoding.text . renderKeyParser)

-- | Labels are key-value pairs that can be attached to all resources.
data Label = Label
  { labelKey :: LabelKey
  , labelValue :: Text
    } deriving (Eq, Show)

-- | A label map maps label keys to values.
type LabelMap = Map LabelKey Text

-- | Build a label map from a list of labels.
toLabelMap :: [Label] -> LabelMap
toLabelMap = foldr (\label -> Map.insert (labelKey label) $ labelValue label) Map.empty

-- | Get a list of labels from a label map.
fromLabelMap :: LabelMap -> [Label]
fromLabelMap = Map.foldrWithKey (\k v xs -> Label k v : xs) []

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

-- | Exception produced while performing a request to Hetzner Cloud.
data CloudException =
    CloudError Error
  | JSONError (HTTP.Response ByteString) String
    deriving Show

instance Exception CloudException

-- | A firewall ID and whether the firewall is applied or not.
data FirewallStatus = FirewallStatus
  { firewallStatusID :: FirewallID
  , firewallIsApplied :: Bool
    } deriving Show

instance FromJSON FirewallStatus where
  parseJSON = JSON.withObject "FirewallStatus" $ \o -> do
    status <- o .: "status"
    liftA2 FirewallStatus (o .: "id") $ case status of
      "applied" -> pure True
      "pending" -> pure False
      _ -> fail $ "Invalid firewall status: " ++ Text.unpack status

-- | Public IP information.
data PublicIPInfo dnsptr ip = PublicIPInfo
  { -- | Reverse DNS PTR entry/entries.
    reverseDNS :: dnsptr
    -- | IP address/range.
  , publicIP :: ip
    } deriving Show

instance (FromJSON dnsptr, FromJSON ip) => FromJSON (PublicIPInfo dnsptr ip) where
  parseJSON = JSON.withObject "PublicIPInfo" $ \o -> PublicIPInfo
    <$> o .: "dns_ptr"
    <*> o .: "ip"

-- | Public network information associated with a 'Server'.
data PublicNetwork = PublicNetwork
  { publicNetworkFirewalls :: [FirewallStatus]
  , publicNetworkFloatingIPs :: [FloatingIPID]
  , publicIPv4 :: Maybe (PublicIPInfo Text IPv4)
  , publicIPv6 :: Maybe (PublicIPInfo [PublicIPInfo Text IPv6] IPv6Range)
    } deriving Show

instance FromJSON PublicNetwork where
  parseJSON = JSON.withObject "PublicNetwork" $ \o -> PublicNetwork
    <$> o .: "firewalls"
    <*> o .: "floating_ips"
    <*> o .: "ipv4"
    <*> o .: "ipv6"

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
  :: (ToJSON body, FromJSON a)
  => ByteString -- ^ Method
  -> ByteString -- ^ Path
  -> Maybe body -- ^ Request body. You may use 'noBody' to skip.
  -> Token -- ^ Authorization token
  -> Maybe Int -- ^ Page
  -> IO a
cloudQuery method path mbody (Token token) mpage = do
  let req = HTTP.setRequestMethod method
          $ HTTP.setRequestSecure True
          $ HTTP.setRequestHost "api.hetzner.cloud"
          $ HTTP.setRequestPort 443
          $ HTTP.setRequestPath ("/v1" <> path)
          $ maybe id HTTP.setRequestBodyJSON mbody
          $ HTTP.addRequestHeader "Authorization" ("Bearer " <> token)
          $ maybe id (\page -> HTTP.addToRequestQueryString
                                 [("page", Just $ fromString $ show page)]) mpage
          $ HTTP.defaultRequest
  resp <- HTTP.httpBS req
  let body = HTTP.getResponseBody resp
  case divMod (HTTP.getResponseStatusCode resp) 100 of
    (2,m) ->
      let body' = if m == 4 then "{}" else body
      in  case JSON.eitherDecodeStrict body' of
            Left err -> throwIO $ JSONError resp err
            Right x -> pure x
    _ -> case JSON.eitherDecodeStrict body of
           Left err -> throwIO $ JSONError resp err
           Right x -> throwIO $ CloudError $ withoutKey @"error" x

-- | Used to send requests without a body.
noBody :: Maybe Void
noBody = Nothing

-- | Stream results using a function that takes a page number,
--   going through all the pages.
streamPages
  :: forall key f a i m
   . (Foldable f, MonadIO m)
  -- | Function that takes page number and returns result.
  => (Maybe Int -> IO (WithMeta key (f a)))
  -- | Conduit-based stream that yields results downstream.
  -> ConduitT i a m ()
streamPages f = go Nothing
  where
    go :: Maybe Int -> ConduitT i a m ()
    go page = do
      resp <- liftIO $ f page
      -- Yield results from response
      forM_ resp $ mapM_ Conduit.yield
      -- Continue if not in last page
      let next = nextPage $ pagination $ responseMeta resp
      if isNothing next then pure () else go next

-- | Convenient function to turn streams into lists.
streamToList :: Monad m => ConduitT () a m () -> m [a]
streamToList = Conduit.sourceToList

-- | Wrap a value with the key of the value within a JSON object.
data WithKey (key :: Symbol) a = WithKey { withoutKey :: a } deriving Show

instance Functor (WithKey key) where
  fmap f (WithKey x) = WithKey (f x)

instance Foldable (WithKey key) where
  foldMap f = f . withoutKey

instance (KnownSymbol key, FromJSON a) => FromJSON (WithKey key a) where
  parseJSON =
    let key = symbolVal (Proxy @key)
    in  JSON.withObject ("WithKey:" ++ key) $ \o ->
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

instance Foldable (WithMeta key) where
  foldMap f = f . withoutMeta

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

----------------------------------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------------------------------

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
    -- | Volume ID.
  | ResourceVolumeID VolumeID
    deriving Show

instance FromJSON ResourceID where
  parseJSON = JSON.withObject "ResourceID" $ \o -> do
    t <- o .: "type"
    case t :: Text of
      "server" -> ResourceServerID <$> o .: "id"
      "volume" -> ResourceVolumeID <$> o .: "id"
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
getActions
  :: Token
  -> Maybe Int -- ^ Page.
  -> IO (WithMeta "actions" [Action])
getActions = cloudQuery "GET" "/actions" noBody

-- | Get a single action.
getAction :: Token -> ActionID -> IO Action
getAction token (ActionID i) = withoutKey @"action" <$>
  cloudQuery "GET" ("/actions/" <> fromString (show i)) noBody token Nothing

-- | Wait until an action is complete and returns the finishing time.
--   It throws a 'CloudException' if the action fails.
waitForAction :: Token -> ActionID -> IO ZonedTime
waitForAction token i = go
  where
    go :: IO ZonedTime
    go = do action <- getAction token i
            case actionStatus action of
              ActionRunning _ -> threadDelay 200000 *> go
              ActionSuccess t -> pure t
              ActionError _ err -> throwIO $ CloudError err

----------------------------------------------------------------------------------------------------
-- Datacenters
----------------------------------------------------------------------------------------------------

-- | Datacenter identifier.
newtype DatacenterID = DatacenterID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | Server types available in a datacenter.
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

-- | A datacenter within a location.
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

-- | Datacenter list with a datacenter recommendation for new servers.
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
getDatacenters token = cloudQuery "GET" "/datacenters" noBody token Nothing

-- | Get a single datacenter.
getDatacenter :: Token -> DatacenterID -> IO Datacenter
getDatacenter token (DatacenterID i) = withoutKey @"datacenter" <$>
  cloudQuery "GET" ("/datacenters/" <> fromString (show i)) noBody token Nothing

----------------------------------------------------------------------------------------------------
-- Firewalls
----------------------------------------------------------------------------------------------------

-- | Firewall identifier.
newtype FirewallID = FirewallID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

----------------------------------------------------------------------------------------------------
-- Floating IPs
----------------------------------------------------------------------------------------------------

-- | Floating IP identifier.
newtype FloatingIPID = FloatingIPID Int deriving (Eq, Ord, Show, FromJSON)

----------------------------------------------------------------------------------------------------
-- Images
----------------------------------------------------------------------------------------------------

-- | Image identifier.
newtype ImageID = ImageID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | Flavor of operative system.
data OSFlavor = Ubuntu | CentOS | Debian | Fedora | Rocky | Alma | UnknownOS deriving Show

instance FromJSON OSFlavor where
  parseJSON = JSON.withText "OSFlavor" $ \t -> case t of
    "ubuntu"  -> pure Ubuntu
    "centos"  -> pure CentOS
    "debian"  -> pure Debian
    "fedora"  -> pure Fedora
    "rocky"   -> pure Rocky
    "alma"    -> pure Alma
    "unknown" -> pure UnknownOS
    _ -> fail $ "Unknown OS flavor: " ++ Text.unpack t

-- | Image type.
data ImageType =
    -- | System image with name.
    SystemImage Text
  | AppImage
    -- | Snapshot with size in GB.
  | Snapshot Double
  | Backup ServerID
  | Temporary
    deriving Show

-- | An image that can be mounted to a server.
data Image = Image
  { imageCreated :: ZonedTime
  , imageDeleted :: Maybe ZonedTime
  , imageDeprecated :: Maybe ZonedTime
  , imageDescription :: Text
    -- | Size of the disk contained in the image in GB.
  , imageDiskSize :: Int
  , imageID :: ImageID
  , imageLabels :: LabelMap
  , imageOSFlavor :: OSFlavor
  , imageType :: ImageType
    } deriving Show

instance FromJSON Image where
  parseJSON = JSON.withObject "Image" $ \o -> do
    typ <- do t <- o .: "type"
              case t :: Text of
                "system" -> SystemImage <$> o .: "name"
                "app" -> pure AppImage
                "snapshot" -> Snapshot <$> o .: "image_size"
                "backup" -> Backup <$> o .: "bound_to"
                "temporary" -> pure Temporary
                _ -> fail $ "Unknown image type: " ++ Text.unpack t
    Image
      <$> o .: "created"
      <*> o .: "deleted"
      <*> o .: "deprecated"
      <*> o .: "description"
      <*> o .: "disk_size"
      <*> o .: "id"
      <*> o .: "labels"
      <*> o .: "os_flavor"
      <*> pure typ

-- | Get images.
getImages
  :: Token
  -> Maybe Int -- ^ Page.
  -> IO (WithMeta "images" [Image])
getImages = cloudQuery "GET" "/images" noBody

-- | Get a single image.
getImage :: Token -> ImageID -> IO Image
getImage token (ImageID i) = withoutKey @"image" <$>
  cloudQuery "GET" ("/images/" <> fromString (show i)) noBody token Nothing

----------------------------------------------------------------------------------------------------
-- Locations
----------------------------------------------------------------------------------------------------

-- | Cities where Hetzner hosts their servers.
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
newtype LocationID = LocationID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | A location.
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
  cloudQuery "GET" "/locations" noBody token Nothing

-- | Get a single location.
getLocation :: Token -> LocationID -> IO Location
getLocation token (LocationID i) = withoutKey @"location" <$>
  cloudQuery "GET" ("/locations/" <> fromString (show i)) noBody token Nothing

----------------------------------------------------------------------------------------------------
-- Pricing
----------------------------------------------------------------------------------------------------

-- | A resource's price.
data Price = Price
  { grossPrice :: Scientific
  , netPrice :: Scientific
    } deriving (Eq, Show)

-- | The 'Ord' instance can be used to compare prices.
--   Only the gross price is used for comparisons.
instance Ord Price where
  compare p p' = compare (grossPrice p) (grossPrice p')

-- | Prices are written as strings. This internal type helps
--   parsing that string in the 'FromJSON' instance.
newtype PriceString = PriceString { fromPriceString :: Scientific }

instance FromJSON PriceString where
  parseJSON = JSON.withText "PriceString" $ \t ->
   either (fail . Parser.errorBundlePretty) (pure . PriceString) $
     Parser.runParser (Parser.scientific :: Parser Scientific) "JSON" t

instance FromJSON Price where
  parseJSON = JSON.withObject "Price" $ \o ->
    liftA2 Price (fromPriceString <$> o .: "gross")
                 (fromPriceString <$> o .: "net")

-- | The price of a resource in a location.
--   Hourly pricing is unavailable for some resources.
data PriceInLocation = PriceInLocation
  { -- | Location name.
    priceLocation :: Text
    -- | Hourly price.
  , hourlyPrice :: Maybe Price
    -- | Monthly price.
  , monthlyPrice :: Price
    } deriving Show

instance FromJSON PriceInLocation where
  parseJSON = JSON.withObject "PriceInLocation" $ \o -> PriceInLocation
    <$> o .:  "location"
    <*> o .:? "price_hourly"
    <*> o .:  "price_monthly"

----------------------------------------------------------------------------------------------------
-- Servers
----------------------------------------------------------------------------------------------------

-- | A server status.
data ServerStatus =
    Running
  | Initializing
  | Starting
  | Stopping
  | Off
  | Deleting
  | Migrating
  | Rebuilding
  | StatusUnknown
    deriving (Eq, Show)

instance FromJSON ServerStatus where
  parseJSON = JSON.withText "ServerStatus" $ \t -> case t of
    "running" -> pure Running
    "initializing" -> pure Initializing
    "starting" -> pure Starting
    "stopping" -> pure Stopping
    "off" -> pure Off
    "deleting" -> pure Deleting
    "migrating" -> pure Migrating
    "rebuilding" -> pure Rebuilding
    "unknown" -> pure StatusUnknown
    _ -> fail $ "Invalid server status: " ++ Text.unpack t

-- | Server identifier.
newtype ServerID = ServerID Int deriving (Show, FromJSON, ToJSON)

-- | A server.
data Server = Server
  { serverCreated :: ZonedTime
  , serverDatacenter :: Datacenter
  , serverID :: ServerID
  , serverImage :: Image
  , serverLabels :: LabelMap
  , serverIsLocked :: Bool
  , serverName :: Text
  , serverPublicNetwork :: PublicNetwork
  , serverType :: ServerType
  , serverStatus :: ServerStatus
    } deriving Show

instance FromJSON Server where
  parseJSON = JSON.withObject "Server" $ \o -> Server
    <$> o .: "created"
    <*> o .: "datacenter"
    <*> o .: "id"
    <*> o .: "image"
    <*> o .: "labels"
    <*> o .: "locked"
    <*> o .: "name"
    <*> o .: "public_net"
    <*> o .: "server_type"
    <*> o .: "status"

-- | Server creation configuration to be used with 'createServer'.
data NewServer = NewServer
  { -- | Automount attached volumes.
    newServerAutomount :: Bool
  , newServerLocation :: Maybe (Either DatacenterID LocationID)
  , newServerFirewalls :: [FirewallID]
  , newServerImage :: ImageID
  , newServerLabels :: [Label]
    -- | Name of the server. Must be unique per project and a valid
    --   hostname as per RFC 1123.
  , newServerName :: Text
  , newServerEnableIPv4 :: Bool
  , newServerEnableIPv6 :: Bool
  , newServerType :: ServerTypeID
  , newServerSSHKeys :: [SSHKeyID]
    -- | Whether to start the server after creation.
  , newServerStart :: Bool
    -- | Volumes to attach to the server after creation.
  , newServerVolumes :: [VolumeID]
    } deriving Show

instance ToJSON NewServer where
  toJSON nserver = JSON.object $ mconcat
    [ pure $ "automount" .= (newServerAutomount nserver && not (null $ newServerVolumes nserver))
    , maybe mempty (pure . either ("datacenter".=) ("location".=)) $ newServerLocation nserver
    , pure $ "firewalls" .= newServerFirewalls nserver
    , pure $ "image" .= newServerImage nserver
    , pure $ "labels" .= toLabelMap (newServerLabels nserver)
    , pure $ "name" .= newServerName nserver
    , pure $ "public_net" .= JSON.object
        [ "enable_ipv4" .= newServerEnableIPv4 nserver
        , "enable_ipv6" .= newServerEnableIPv6 nserver
          ]
    , pure $ "server_type" .= newServerType nserver
    , pure $ "ssh_keys" .= newServerSSHKeys nserver
    , pure $ "start_after_create" .= newServerStart nserver
    , pure $ "volumes" .= newServerVolumes nserver
      ]

-- | Default server configuration that can be used as a starting point
--   for a custom server configuration.
--
--   Note that by default no SSH key is installed, which means you'll need the
--   password in the response in order to access the server (you will also receive an
--   e-mail with the password).
--
defaultNewServer
  :: Text -- ^ Server name.
  -> NewServer
defaultNewServer name = NewServer
  { newServerAutomount = True
  , newServerLocation = Nothing
  , newServerFirewalls = []
  , newServerImage = ImageID 67794396
  , newServerLabels = []
  , newServerName = name
  , newServerEnableIPv4 = True
  , newServerEnableIPv6 = True
  , newServerType = ServerTypeID 1
  , newServerSSHKeys = []
  , newServerStart = True
  , newServerVolumes = []
    }

-- | A server that was just created with 'createServer'.
data CreatedServer = CreatedServer
  { -- | Server creation action. You can use 'waitForAction'
    --   to wait until the server creation is finished.
    createdServerAction :: Action
    -- | Additional server actions that are run after the server
    --   is created, like mounting volumes or starting the server.
  , createdServerNextActions :: [Action]
    -- | Root password returned when no SSH keys are provided.
  , createdServerPassword :: Maybe Text
    -- | The server being created.
  , createdServer :: Server
    } deriving Show

instance FromJSON CreatedServer where
  parseJSON = JSON.withObject "CreatedServer" $ \o -> CreatedServer
    <$> o .: "action"
    <*> o .: "next_actions"
    <*> o .: "root_password"
    <*> o .: "server"

-- | Get servers.
getServers
  :: Token
  -> Maybe Int -- ^ Page.
  -> IO (WithMeta "servers" [Server])
getServers = cloudQuery "GET" "/servers" noBody

-- | Get a single server.
getServer :: Token -> ServerID -> IO Server
getServer token (ServerID i) = withoutKey @"server" <$>
  cloudQuery "GET" ("/servers/" <> fromString (show i)) noBody token Nothing

-- | Create a new server.
createServer :: Token -> NewServer -> IO CreatedServer
createServer token nserver =
  cloudQuery "POST" "/servers" (Just nserver) token Nothing

-- | Delete a server.
deleteServer :: Token -> ServerID -> IO Action
deleteServer token (ServerID i) = withoutKey @"action" <$>
  cloudQuery "DELETE" ("/servers/" <> fromString (show i)) noBody token Nothing

----------------------------------------------------------------------------------------------------
-- Server Types
----------------------------------------------------------------------------------------------------

-- | Computer architecture.
data Architecture = X86 | Arm deriving (Eq, Show)

instance FromJSON Architecture where
  parseJSON = JSON.withText "Architecture" $ \t -> case t of
    "x86" -> pure X86
    "arm" -> pure Arm
    _ -> fail $ "Unknown architecture: " ++ Text.unpack t

-- | Type of server boot drive.
data StorageType = LocalStorage | NetworkStorage deriving (Eq, Show)

instance FromJSON StorageType where
  parseJSON = JSON.withText "StorageType" $ \t -> case t of
    "local" -> pure LocalStorage
    "network" -> pure NetworkStorage
    _ -> fail $ "Unknown storage type: " ++ Text.unpack t

-- | CPU types available.
data CPUType = SharedCPU | DedicatedCPU deriving (Eq, Show)

instance FromJSON CPUType where
  parseJSON = JSON.withText "CPUType" $ \t -> case t of
    "shared" -> pure SharedCPU
    "dedicated" -> pure DedicatedCPU
    _ -> fail $ "Unknown CPU type: " ++ Text.unpack t

-- | Server type identifier.
newtype ServerTypeID = ServerTypeID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | Server characteristics.
data ServerType = ServerType
  { serverArchitecture :: Architecture
  , serverCores :: Int
  , serverCPUType :: CPUType
  , serverDeprecated :: Bool
  , serverTypeDescription :: Text
    -- | Disk size a server of this type has in GB.
  , serverDisk :: Int
  , serverTypeID :: ServerTypeID
    -- | Memory a server of this type has in GB.
  , serverMemory :: Int
  , serverTypeName :: Text
  , serverPricing :: [PriceInLocation]
  , serverStorageType :: StorageType
    } deriving Show

instance FromJSON ServerType where
  parseJSON = JSON.withObject "ServerType" $ \o -> ServerType
    <$> o .: "architecture"
    <*> o .: "cores"
    <*> o .: "cpu_type"
    <*> (fromMaybe False <$> o .: "deprecated")
    <*> o .: "description"
    <*> o .: "disk"
    <*> o .: "id"
    <*> o .: "memory"
    <*> o .: "name"
    <*> o .: "prices"
    <*> o .: "storage_type"

-- | Get all server types.
getServerTypes :: Token -> IO [ServerType]
getServerTypes token = withoutKey @"server_types" <$>
  cloudQuery "GET" "/server_types" noBody token Nothing

----------------------------------------------------------------------------------------------------
-- SSH Keys
----------------------------------------------------------------------------------------------------

-- | SSH key identifier.
newtype SSHKeyID = SSHKeyID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | SSH key information.
data SSHKey = SSHKey
  { sshKeyCreated :: ZonedTime
  , sshKeyFingerprint :: Fingerprint
  , sshKeyID :: SSHKeyID
  , sshKeyLabels :: LabelMap
  , sshKeyName :: Text
  , sshKeyPublicKey :: Text
    } deriving Show

instance FromJSON SSHKey where
  parseJSON = JSON.withObject "SSHKey" $ \o -> SSHKey
    <$> o .: "created"
    <*> o .: "fingerprint"
    <*> o .: "id"
    <*> o .: "labels"
    <*> o .: "name"
    <*> o .: "public_key"

-- | Get all uploaded SSH keys.
getSSHKeys :: Token -> IO [SSHKey]
getSSHKeys token = withoutKey @"ssh_keys" <$>
  cloudQuery "GET" "/ssh_keys" noBody token Nothing

-- | Get a single SSH key.
getSSHKey :: Token -> SSHKeyID -> IO SSHKey
getSSHKey token (SSHKeyID i) = withoutKey @"ssh_key" <$>
  cloudQuery "GET" ("/ssh_keys/" <> fromString (show i)) noBody token Nothing

-- | Upload an SSH key.
createSSHKey
  :: Token
  -> Text -- ^ Name for the SSH key.
  -> Text -- ^ Public key.
  -> [Label] -- ^ List of labels to attach to the key.
  -> IO SSHKey
createSSHKey token name public labels = withoutKey @"ssh_key" <$>
  let body = JSON.object
        [ "labels" .= toLabelMap labels
        , "name" .= name
        , "public_key" .= public
          ]
  in  cloudQuery "POST" "/ssh_keys" (Just body) token Nothing

-- | Delete an SSH key.
deleteSSHKey :: Token -> SSHKeyID -> IO ()
deleteSSHKey token (SSHKeyID i) =
  cloudQuery "DELETE" ("/ssh_keys/" <> fromString (show i)) noBody token Nothing

-- | Update name and labels of an SSH key.
updateSSHKey
  :: Token
  -> SSHKeyID
  -> Text -- ^ New name for the key.
  -> [Label] -- ^ New labels for the key.
  -> IO SSHKey -- ^ Updated SSH key.
updateSSHKey token (SSHKeyID i) name labels = withoutKey @"ssh_key" <$>
  let body = JSON.object
        [ "labels" .= toLabelMap labels
        , "name" .= name
          ]
  in  cloudQuery "PUT" ("/ssh_keys/" <> fromString (show i)) (Just body) token Nothing

----------------------------------------------------------------------------------------------------
-- Volumes
----------------------------------------------------------------------------------------------------

-- | Volume identifier.
newtype VolumeID = VolumeID Int deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | Volume format.
data VolumeFormat = EXT4 | XFS deriving (Eq, Show)

instance FromJSON VolumeFormat where
  parseJSON = JSON.withText "VolumeFormat" $ \t -> case t of
    "ext4" -> pure EXT4
    "xfs" -> pure XFS
    _ -> fail $ "Invalid volume format: " ++ Text.unpack t

instance ToJSON VolumeFormat where
  toJSON EXT4 = JSON.String "ext4"
  toJSON XFS = JSON.String "xfs"

-- | Volume status.
data VolumeStatus = VolumeCreating | VolumeAvailable deriving (Eq, Show)

instance FromJSON VolumeStatus where
  parseJSON = JSON.withText "VolumeStatus" $ \t -> case t of
    "creating" -> pure VolumeCreating
    "available" -> pure VolumeAvailable
    _ -> fail $ "Invalid volume status: " ++ Text.unpack t

-- | A volume that can be attached to a server.
data Volume = Volume
  { volumeCreated :: ZonedTime
    -- | Volume format. It returns 'Nothing' if the volume hasn't been formatted yet.
  , volumeFormat :: Maybe VolumeFormat
  , volumeID :: VolumeID
  , volumeLabels :: LabelMap
    -- | Device path on the file system for the volume.
  , volumePath :: FilePath
  , volumeLocation :: Location
  , volumeName :: Text
    -- | ID of the server the volume is attached to, if any.
  , volumeServer :: Maybe ServerID
    -- | Size of the volume in GB.
  , volumeSize :: Int
  , volumeStatus :: VolumeStatus
    } deriving Show

instance FromJSON Volume where
  parseJSON = JSON.withObject "Volume" $ \o -> Volume
    <$> o .: "created"
    <*> o .: "format"
    <*> o .: "id"
    <*> o .: "labels"
    <*> o .: "linux_device"
    <*> o .: "location"
    <*> o .: "name"
    <*> o .: "server"
    <*> o .: "size"
    <*> o .: "status"

-- | Attach a volume to a server. The boolean parameter
--   indicates whether the volume will be auto-mounted.
data AttachToServer = AttachToServer ServerID Bool

-- | Volume creation configuration to be used with 'createVolume'.
data NewVolume = NewVolume
  { -- | If specified, volume will be formatted according
    --   to the given format.
    newVolumeFormat :: Maybe VolumeFormat
  , newVolumeLabels :: [Label]
    -- | You can either create a volume in a location or
    --   directly attach the volume to a server.
  , newVolumeLocation :: Either LocationID AttachToServer
  , newVolumeName :: Text
    -- | Size of the volume in GB. It must be at least 10.
  , newVolumeSize :: Int
    }

instance ToJSON NewVolume where
  toJSON nvolume = JSON.object $ mconcat
    [ maybe mempty (pure . ("format".=)) $ newVolumeFormat nvolume
    , pure $ "labels" .= toLabelMap (newVolumeLabels nvolume)
    , let f :: AttachToServer -> [JSON.Pair]
          f (AttachToServer i b) = [ "server" .= i, "automount" .= b ]
      in  either (pure . ("location".=)) f $ newVolumeLocation nvolume
    , pure $ "name" .= newVolumeName nvolume
    , pure $ "size" .= newVolumeSize nvolume
      ]

-- | A volume created with 'createVolume'.
data CreatedVolume = CreatedVolume
  { createdVolumeAction :: Action
  , createdVolumeNextActions :: [Action]
  , createdVolume :: Volume
    } deriving Show

instance FromJSON CreatedVolume where
  parseJSON = JSON.withObject "CreatedVolume" $ \o -> CreatedVolume
    <$> o .: "action"
    <*> o .: "next_actions"
    <*> o .: "volume"

-- | Get volumes.
getVolumes :: Token -> Maybe Int -> IO (WithMeta "volumes" [Volume])
getVolumes = cloudQuery "GET" "/volumes" noBody

-- | Get a single volume.
getVolume :: Token -> VolumeID -> IO Volume
getVolume token (VolumeID i) = withoutKey @"volume" <$>
  cloudQuery "GET" ("/volumes/" <> fromString (show i)) noBody token Nothing

-- | Create a new volume.
createVolume :: Token -> NewVolume -> IO CreatedVolume
createVolume token nvolume =
  cloudQuery "POST" "/volumes" (Just nvolume) token Nothing

-- | Delete a volume.
deleteVolume :: Token -> VolumeID -> IO ()
deleteVolume token (VolumeID i) =
  cloudQuery "DELETE" ("/volumes/" <> fromString (show i)) noBody token Nothing

-- | Update name and labels of a volume.
updateVolume
  :: Token
  -> VolumeID
  -> Text -- ^ New name for the volume.
  -> [Label] -- ^ New labels for the volume.
  -> IO Volume -- ^ Updated volume.
updateVolume token (VolumeID i) name labels = withoutKey @"volume" <$>
  let body = JSON.object
        [ "labels" .= toLabelMap labels
        , "name" .= name
          ]
  in  cloudQuery "PUT" ("/volumes/" <> fromString (show i)) (Just body) token Nothing
