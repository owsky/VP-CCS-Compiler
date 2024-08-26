module CCS.Parser.Action where

-- import CCS.Lexer.Utils (Parser, backslash, comma, curlyParens, lexeme, slash, squareParens, symbol, tick)
-- import CCS.Parser.Grammars (Action (..), Label (..), RelabellingFunction (..), RelabellingMapping (..), getLabelName)
-- import Data.Set (Set, fromList)
-- import Text.Megaparsec (choice, sepBy, some, (<?>))
-- import Text.Megaparsec.Char (letterChar)

-- -- | Parser for action relabelling functions
-- pActionRelabel :: Parser RelabellingMapping
-- pActionRelabel = do
--   ogLabel <- pLabel
--   _ <- slash
--   newLabel <- pLabel
--   return RelabellingMapping {from = getLabelName ogLabel, to = getLabelName newLabel}

-- -- | Parser for a list of relabelling functions
-- pRelabellingFunction :: Parser RelabellingFunction
-- pRelabellingFunction = squareParens $ do
--   m <- pActionRelabel `sepBy` comma
--   return RelabellingFunction {mappings = m}

-- pActionRestriction :: Parser (Set Label)
-- pActionRestriction = do
--   _ <- backslash
--   labels <- curlyParens $ pLabel `sepBy` comma
--   return (fromList labels)
