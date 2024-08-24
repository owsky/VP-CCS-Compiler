module Utils where

import Control.Monad.Combinators.Expr (Operator (InfixL))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, empty)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

-- | Helper type which parses strings without transformers
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

-- | Parses content between round braces
roundParens :: Parser a -> Parser a
roundParens = between (symbol "(") (symbol ")")

-- | Parses content between square braces
squareParens :: Parser a -> Parser a
squareParens = between (symbol "[") (symbol "]")

-- | Parses the content between curly braces
curlyParens :: Parser a -> Parser a
curlyParens = between (symbol "{") (symbol "}")

-- | Parses the dot character
dot :: Parser Text
dot = symbol "."

-- | Parses the comma character
comma :: Parser Text
comma = symbol ","

-- | Parses the forward slash character
slash :: Parser Text
slash = symbol "/"

-- | Parses the backslash character
backslash :: Parser Text
backslash = symbol "\\"

-- | Parses the tick character
tick :: Parser Text
tick = symbol "'"

-- | Creates a polymorphic, left-associative, binary operator
binary :: forall a. Text -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)