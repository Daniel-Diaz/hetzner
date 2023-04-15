
module Hetzner.Cloud.Internal
  ( -- * Parsing
    Parser
    -- ** IP
  , ipParser
    ) where

-- base
import Data.Void
-- megaparsec
import Text.Megaparsec qualified as Parser
import Text.Megaparsec.Char.Lexer qualified as Parser
-- text
import Data.Text (Text)
-- network
import Network.Socket (HostAddress, tupleToHostAddress)

type Parser = Parser.Parsec Void Text

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
