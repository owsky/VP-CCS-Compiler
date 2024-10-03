module Parser.Parse where

import Control.Applicative (some)
import Data.Text (Text)
import Grammars.Pure_AST (Statement)
import Parser.StatementParser (parseStatement)
import Parser.Utils (Parser)
import Text.Megaparsec (eof, errorBundlePretty, parse)

-- | Attempts to parse the given text into a list of statements
parseInput :: Int -> Text -> Either String [[Statement]]
parseInput maxInt input = do
  let parsed = parse (parseLines maxInt) "" input
  case parsed of
    Left bundle -> Left $ errorBundlePretty bundle
    Right statements -> Right statements

-- | Parser for multiple statements, separated by eol characters
parseLines :: Int -> Parser [[Statement]]
parseLines maxInt = (some $ parseStatement maxInt) <* eof