module Parser.BExprParser where

import AST (BExpr (..))
import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Parser.AExprParser (pAExpr)
import Parser.Utils (Parser, binaryL, lexeme, prefix, roundParens, symbol)
import Text.Megaparsec (MonadParsec (try), choice, (<?>))
import Text.Megaparsec.Char (string)

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