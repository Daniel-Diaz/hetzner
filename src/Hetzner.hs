---------------------------------------------------------------------------------------------------

-- | Hetzner Cloud API client.
--
--   Official documentation at https://docs.hetzner.cloud.
module Hetzner
  ( -- * Token
    Token (..)
    -- * Errors
  , ErrorCode (..)
  , Error (..)
    ) where

-- base
import Data.Foldable (find)
import Data.Char (isUpper, toLower)
-- text
import Data.Text (Text)
import Data.Text qualified as Text
-- aeson
import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import Data.Aeson qualified as JSON

-- | A token used to authenticate requests.
--
--   You can obtain one through the [Hetzner Cloud Console](https://console.hetzner.cloud).
newtype Token = Token Text

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
