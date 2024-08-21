module Utils where

import Control.Monad.Combinators.Expr (Operator (InfixL))
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, empty)
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as L

-- | Helper type which parses strings without transformers
type Parser = Parsec Void String

-- | Space consumer
sc :: Parser ()
sc = L.space space1 empty empty

-- | Uses the given parser to parse the lexemes
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Tokenizes the given string
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Parses content between round braces
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parses the dot character
dot :: Parser Char
dot = char '.'

-- | Creates a binary Operator parser for makeExprParser's operator table
binary :: forall a. String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)