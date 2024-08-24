module CCS.Parser.Action where

import CCS.Parser.Grammars (Action (..), Label (..), RelabellingFunction (..), RelabellingMapping (..), getLabelName)
import Data.Set (Set, fromList)
import Text.Megaparsec (choice, sepBy, some, (<?>))
import Text.Megaparsec.Char (letterChar)
import Utils (Parser, backslash, comma, curlyParens, lexeme, slash, squareParens, symbol, tick)

-- | Parser for labels
pLabel :: Parser Label
pLabel = choice [Output <$> (tick *> lexeme (some letterChar)), Input <$> lexeme (some letterChar)]

-- | Parser for actions
pAction :: Parser Action
pAction = choice [Tau <$ lexeme (symbol "τ"), ActionName <$> lexeme pLabel <?> "Action"]

-- | Parser for action relabelling functions
pActionRelabel :: Parser RelabellingMapping
pActionRelabel = do
  ogLabel <- pLabel
  _ <- slash
  newLabel <- pLabel
  return RelabellingMapping {from = getLabelName ogLabel, to = getLabelName newLabel}

-- | Parser for a list of relabelling functions
pRelabellingFunction :: Parser RelabellingFunction
pRelabellingFunction = squareParens $ do
  m <- pActionRelabel `sepBy` comma
  return RelabellingFunction {mappings = m}

pActionRestriction :: Parser (Set Label)
pActionRestriction = do
  _ <- backslash
  labels <- curlyParens $ pLabel `sepBy` comma
  return (fromList labels)
