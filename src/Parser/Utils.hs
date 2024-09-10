module Parser.Utils where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR, Prefix))
import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, between, lookAhead, try, (<|>))
import Text.Megaparsec.Char (space1, string)
import Text.Megaparsec.Char.Lexer qualified as L

-- | Helper type which parses strings without transformers
type Parser = Parsec Void Text

-- | Space consumer
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

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

-- | Parses the comma character
comma :: Parser Text
comma = symbol ","

-- | Parses the forward slash character
slash :: Parser Text
slash = symbol "/"

-- | Creates a polymorphic prefix operator
prefix :: forall a. Text -> (a -> a) -> Operator Parser a
prefix name f = Prefix (f <$ symbol name)

-- | Creates a polymorphic, left-associative, binary operator
binaryL :: forall a. Text -> (a -> a -> a) -> Operator Parser a
binaryL name f = InfixL (f <$ symbol name)

-- | Creates a polymorphic, right-associative, binary operator
binaryR :: forall a. Text -> (a -> a -> a) -> Operator Parser a
binaryR name f = InfixR (f <$ symbol name)

-- | Creates a polymorphic, left-associative, binary operator that does not consume the operator it matches
binaryL' :: forall a. Text -> (a -> a -> a) -> Operator Parser a
binaryL' name f = InfixL (f <$ (try (lookAhead (symbol name)) $> ()))

-- | Parses a decimal number
decimal :: Parser Int
decimal = lexeme L.decimal

-- | Tries to parse the given word, making sure that it's either followed by whitespace or an end of input
pWord :: Text -> Parser Text
pWord word = try $ string word <* (void space1 <|> eof)