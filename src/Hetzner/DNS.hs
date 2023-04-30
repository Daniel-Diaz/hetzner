----------------------------------------------------------------------------------------------------

-- | Client for the Hetzner DNS API.
module Hetzner.DNS (
    -- * Tokens
    Token (..)
  , getTokenFromEnv
    -- * Hetzner DNS
    -- ** Zones
  , ZoneID (..)
  , ZoneStatus (..)
  , Zone (..)
  , getZones
  , getZone
  , updateZone
  , deleteZone
    -- * Exceptions
  , DNSException (..)
    -- * Streaming
  , streamPages
  , streamToList
    -- * Generic interface
    -- ** Generic queries
  , dnsQuery
  , noBody
    -- ** JSON Wrappers
  , WithKey (..)
  , WithMeta (..)
    -- ** Response metadata
  , ResponseMeta (..)
  , Pagination (..)
  ) where

import Hetzner.Cloud
  ( WithKey (..), WithMeta (..)
  , ResponseMeta (..), Pagination (..), noBody
  , streamPages, streamToList
    )
-- base
import Data.String (IsString, fromString)
import Data.Maybe (maybeToList)
import System.Environment qualified as System
import Control.Exception (Exception, throwIO)
-- bytestring
import Data.ByteString (ByteString)
-- aeson
import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import Data.Aeson qualified as JSON
-- http-conduit
import Network.HTTP.Simple as HTTP
-- time
import Data.Time (ZonedTime, parseTimeM, defaultTimeLocale)
-- text
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)

-- | A token used to authenticate requests.
--
--   You can create one in the [Hetzner DNS Console](https://dns.hetzner.com/settings/api-token).
newtype Token = Token ByteString deriving (Show, Eq, Ord)

instance IsString Token where
  fromString = Token . fromString

-- | Lookup 'Token' from the environment variable @HETZNER_DNS_TOKEN@.
getTokenFromEnv :: IO (Maybe Token)
getTokenFromEnv = fmap fromString <$> System.lookupEnv "HETZNER_DNS_TOKEN"

-- | Exception produced while performing a request to Hetzner DNS.
data DNSException =
    DNSError (HTTP.Response ByteString)
  | JSONError (HTTP.Response ByteString) String
    deriving Show

instance Exception DNSException

-- | Generic Hetzner DNS query.
dnsQuery
  :: (ToJSON body, FromJSON a)
  => ByteString -- ^ Method.
  -> ByteString -- ^ Path.
  -> Maybe body -- ^ Request body. You may use 'noBody' to skip.
  -> Token -- ^ Authorization token.
  -> Maybe Int -- ^ Page.
  -> IO a
dnsQuery method path mbody (Token token) mpage = do
  let req = HTTP.setRequestMethod method
          $ HTTP.setRequestSecure True
          $ HTTP.setRequestHost "dns.hetzner.com"
          $ HTTP.setRequestPort 443
          $ HTTP.setRequestPath ("/api/v1" <> path)
          $ maybe id HTTP.setRequestBodyJSON mbody
          $ HTTP.addRequestHeader "Auth-API-Token" token
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
    _ -> throwIO $ DNSError resp

----------------------------------------------------------------------------------------------------
-- Time parser
----------------------------------------------------------------------------------------------------

newtype DNSTime = DNSTime { dnsTime :: ZonedTime }

instance FromJSON DNSTime where
  parseJSON = JSON.withText "DNSTime" $
    let format = "%F %T%Q %z %Z"
    in  fmap DNSTime . parseTimeM False defaultTimeLocale format . Text.unpack

----------------------------------------------------------------------------------------------------
-- Zones
----------------------------------------------------------------------------------------------------

-- | Zone identifier.
newtype ZoneID = ZoneID Text deriving (Eq, Ord, Show, FromJSON, ToJSON)

-- | Status of a 'Zone'.
data ZoneStatus = Verified | Failed | Pending deriving Show

instance FromJSON ZoneStatus where
  parseJSON = JSON.withText "ZoneStatus" $ \t -> case t of
    "verified" -> pure Verified
    "failed" -> pure Failed
    "pending" -> pure Pending
    _ -> fail $ "Invalid zone status: " ++ Text.unpack t

-- | DNS zone.
data Zone = Zone
  { zoneCreated :: ZonedTime
  , zoneModified :: ZonedTime
  , zoneID :: ZoneID
  , zoneName :: Text
  , zoneIsSecondary :: Bool
  , zoneStatus :: ZoneStatus
  , zoneRecordCount :: Int
  , zoneTTL :: Int
    } deriving Show

instance FromJSON Zone where
  parseJSON = JSON.withObject "Zone" $ \o -> Zone
    <$> (dnsTime <$> o .: "created")
    <*> (dnsTime <$> o .: "modified")
    <*> o .: "id"
    <*> o .: "name"
    <*> o .: "is_secondary_dns"
    <*> o .: "status"
    <*> o .: "records_count"
    <*> o .: "ttl"

-- | Get zones.
getZones :: Token -> Maybe Int -> IO (WithMeta "zones" [Zone])
getZones = dnsQuery "GET" "/zones" noBody

-- | Get a single zone.
getZone :: Token -> ZoneID -> IO Zone
getZone token (ZoneID i) = withoutKey @"zone" <$>
  dnsQuery "GET" ("/zones/" <> encodeUtf8 i) noBody token Nothing

-- | Update a zone's name and TTL.
updateZone
  :: Token
  -> ZoneID -- ^ ID of zone to update.
  -> Text -- ^ New zone name.
  -> Maybe Int -- ^ New TTL. If not provided, it won't change.
  -> IO Zone
updateZone token (ZoneID i) name mttl = withoutKey @"zone" <$>
  let body = JSON.object $ ("name" .= name) : maybeToList (fmap ("ttl" .=) mttl)
  in  dnsQuery "PUT" ("/zones/" <> encodeUtf8 i) (Just body) token Nothing

-- | Delete a zone.
deleteZone :: Token -> ZoneID -> IO ()
deleteZone token (ZoneID i) = dnsQuery "DELETE" ("/zones/" <> encodeUtf8 i) noBody token Nothing
