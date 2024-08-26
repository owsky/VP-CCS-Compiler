module CCS.Parser.Utils where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, empty)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

-- | Space consumer
sc :: Parser ()
sc = L.space space1 empty empty

-- | Consumes any trailing whitespace after parsing the given parser.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Consumes any trailing whitespace after parsing the given character
symbol :: Text -> Parser Text
symbol = L.symbol sc