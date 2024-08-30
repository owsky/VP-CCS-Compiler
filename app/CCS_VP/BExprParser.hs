module CCS_VP.BExprParser where

import CCS_VP.AExprParser (pAExpr)
import CCS_VP.Grammars (BExpr (..))
import CCS_VP.Utils (Parser, binaryL, lexeme, prefix, roundParens, sc, symbol)
import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, try), ParseErrorBundle, choice, parse, (<?>))
import Text.Megaparsec.Char (string)

parseBooleanExpression :: Text -> Either (ParseErrorBundle Text Void) BExpr
parseBooleanExpression = parse (sc *> pBExpr <* eof) ""

pBExpr :: Parser BExpr
pBExpr = makeExprParser pTerm operatorTable

pTerm :: Parser BExpr
pTerm = choice [roundParens pBExpr, pBVal, pBEq, pBLeq]

operatorTable :: [[Operator Parser BExpr]]
operatorTable =
  [ [prefix "!" Not],
    [binaryL "&&" And],
    [binaryL "||" Or]
  ]

pBTrue :: Parser Bool
pBTrue = True <$ lexeme (string "tt") <?> "Boolean true"

pBFalse :: Parser Bool
pBFalse = False <$ lexeme (string "ff") <?> "Boolean false"

pBVal :: Parser BExpr
pBVal = BVal <$> choice [pBTrue, pBFalse] <?> "Boolean value"

pBEq :: Parser BExpr
pBEq = try $ do
  t1 <- pAExpr
  _ <- symbol "=="
  Eq t1 <$> pAExpr

pBLeq :: Parser BExpr
pBLeq = try $ do
  t1 <- pAExpr
  _ <- symbol "<="
  Leq t1 <$> pAExpr