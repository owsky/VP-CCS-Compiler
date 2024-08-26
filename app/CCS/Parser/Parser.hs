module CCS.Parser.Parser where

import CCS.Lexer.Lexer (Token (..))
import CCS.Lexer.Utils (Parser, comma, slash, squareParens, symbol)
import CCS.Parser.Grammars (Action (..), Label (..), Process (..), RelabellingFunction (..), RelabellingMapping (..), getLabelName)
import CCS.Parser.Utils (lexeme)
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (choice, eof, parse, sepBy, some, (<?>))
import Text.Megaparsec.Char (char, letterChar)
import Text.Megaparsec.Error (ParseErrorBundle)

pLabelInput :: Parser Label
pLabelInput = Input . pack <$> lexeme (some letterChar)

pLabelOutput :: Parser Label
pLabelOutput = Output . pack <$> lexeme ((:) <$> char '\'' <*> some letterChar)

pLabel :: Parser Label
pLabel = choice [pLabelOutput, pLabelInput]

pTau :: Parser Action
pTau = Tau <$ symbol "τ"

pAction :: Parser Action
pAction = choice [pTau, ActionName <$> lexeme pLabel <?> "Action"]

parseAction :: Text -> Either (ParseErrorBundle Text Void) Action
parseAction = parse (pAction <* eof) ""

tokenToAction :: Token -> Maybe Action
tokenToAction token = case token of
  TExpr text -> do
    let action = parseAction text
    case action of
      Left _ -> Nothing
      Right a -> Just a
  _ -> Nothing

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

parseRelabellingFunction :: Text -> Either (ParseErrorBundle Text Void) RelabellingFunction
parseRelabellingFunction = parse (pRelabellingFunction <* eof) ""

tokenToRelabelFn :: Token -> Maybe RelabellingFunction
tokenToRelabelFn token = case token of
  TExpr text -> do
    let fn = parseRelabellingFunction text
    case fn of
      Left _ -> Nothing
      Right f -> Just f
  _ -> Nothing

tokenToLabelSet :: Token -> Maybe (Set Label)
tokenToLabelSet token = case token of
  TExpr text -> do
    undefined
  _ -> Nothing

tokenToProcess :: Token -> Maybe Process
tokenToProcess (TExpr text) = Just (ProcessName text)
tokenToProcess (TPre left right) = do
  action <- tokenToAction left
  process <- tokenToProcess right
  return $ ActionPrefix action process
tokenToProcess (TSum left right) = do
  p1 <- tokenToProcess left
  p2 <- tokenToProcess right
  return $ Sum p1 p2
tokenToProcess (TPar left right) = do
  p1 <- tokenToProcess left
  p2 <- tokenToProcess right
  return $ Parallel p1 p2
tokenToProcess (TRel left right) = do
  process <- tokenToProcess left
  relabelFn <- tokenToRelabelFn right
  return $ Relabelling process relabelFn
tokenToProcess (TRes left right) = do
  process <- tokenToProcess left
  labelSet <- tokenToLabelSet right
  return $ Restriction process labelSet
tokenToProcess (TAss left right) = do
  lhv <- tokenToProcess left
  rhv <- tokenToProcess right
  case lhv of
    ProcessName name -> Just $ Assignment name rhv
    _ -> Nothing