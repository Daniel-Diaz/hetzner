{-# OPTIONS_GHC -Wno-orphans #-}

module Hetzner.Cloud.Fingerprint (
    Fingerprint
  , fingerprintParser
  ) where

-- base
import GHC.Fingerprint (Fingerprint (..))
import Data.Void
import Data.Word
import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import Data.Foldable (foldl')
import Data.Bits (shiftL, (.|.))
-- text
import Data.Text (Text)
-- megaparsec
import Text.Megaparsec qualified as Parser
import Text.Megaparsec.Char.Lexer qualified as Parser
-- aeson
import Data.Aeson (FromJSON)
import Data.Aeson qualified as JSON

type Parser = Parser.Parsec Void Text

fingerprintParser :: Parser Fingerprint
fingerprintParser = do
  bs0 <- liftA2 (:) Parser.hexadecimal $
         replicateM 7 $ Parser.single ':' *> Parser.hexadecimal
  bs1 <- replicateM 8 $ Parser.single ':' *> Parser.hexadecimal
  let f :: Word64 -> (Int, Word8) -> Word64
      f acc (i,b) = acc .|. (fromIntegral b `shiftL` (8 * (7 - i)))
      combineBytes :: [Word8] -> Word64
      combineBytes = foldl' f 0 . zip [0..]
  pure $ Fingerprint (combineBytes bs0) (combineBytes bs1)

instance FromJSON Fingerprint where
  parseJSON = JSON.withText "Fingerprint" $ \t ->
    either (fail . Parser.errorBundlePretty) pure $
      Parser.runParser (fingerprintParser <* Parser.eof) "JSON input" t
