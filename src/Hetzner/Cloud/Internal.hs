
module Hetzner.Cloud.Internal
  ( -- * Parsing
    Parser
    -- ** IP
  , ipParser
  , parseIP
    -- * Rendering
  , renderIP
    ) where

-- base
import Data.Void
import Data.Bifunctor (first)
-- megaparsec
import Text.Megaparsec qualified as Parser
import Text.Megaparsec.Char.Lexer qualified as Parser
-- text
import Data.Text (Text)
import Data.Text qualified as Text
-- network
import Network.Socket
  ( HostAddress
  , tupleToHostAddress
  , hostAddressToTuple
    )

-- | Parser with strict text as stream.
type Parser = Parser.Parsec Void Text

-- | IP parser.
ipParser :: Parser HostAddress
ipParser = do
  w1 <- Parser.decimal
  _  <- Parser.single '.'
  w2 <- Parser.decimal
  _  <- Parser.single '.'
  w3 <- Parser.decimal
  _  <- Parser.single '.'
  w4 <- Parser.decimal
  pure $ tupleToHostAddress (w1,w2,w3,w4)

-- | Parse an IP from a strict text.
--   It expects end of input after the IP.
--   Use 'ipParser' if you want to parse an IP that is part
--   of a bigger encoding.
parseIP :: Text -> Either String HostAddress
parseIP t =
    first Parser.errorBundlePretty
  $ Parser.runParser (ipParser <* Parser.eof) "input" t

-- | Render an IP as text.
renderIP :: HostAddress -> Text
renderIP addr =
  let (w1,w2,w3,w4) = hostAddressToTuple addr
  in  Text.intercalate "." $ fmap (Text.pack . show) [w1,w2,w3,w4]
