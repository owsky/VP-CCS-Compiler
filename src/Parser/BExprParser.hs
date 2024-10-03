module Parser.BExprParser (pBExpr) where

import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Grammars.AST (BExpr (..))
import Parser.AExprParser (pAExpr)
import Parser.Utils (Parser, binaryL, lexeme, prefix, roundParens, symbol)
import Text.Megaparsec (MonadParsec (try), choice, (<?>))
import Text.Megaparsec.Char (string)

-- | Boolean expression parser
pBExpr :: Parser BExpr
pBExpr = makeExprParser pTerm operatorTable

pTerm :: Parser BExpr
pTerm = choice [roundParens pBExpr, pBVal, pBEq, pBLt, pBGt]

operatorTable :: [[Operator Parser BExpr]]
operatorTable =
  [ [prefix "!" Not],
    [binaryL "&&" And],
    [binaryL "||" Or]
  ]

-- | Parser for the True Boolean literal
pBTrue :: Parser Bool
pBTrue = True <$ lexeme (string "tt") <?> "Boolean true"

-- | Parser for the False Boolean literal
pBFalse :: Parser Bool
pBFalse = False <$ lexeme (string "ff") <?> "Boolean false"

-- | Parser for Boolean values
pBVal :: Parser BExpr
pBVal = BVal <$> choice [pBTrue, pBFalse] <?> "Boolean value"

-- | Parser for Boolean equality
pBEq :: Parser BExpr
pBEq = try $ do
  t1 <- pAExpr
  _ <- symbol "=="
  Eq t1 <$> pAExpr

-- | Parser for Boolean inequality
pBLt :: Parser BExpr
pBLt = try $ do
  t1 <- pAExpr
  _ <- symbol "<"
  Lt t1 <$> pAExpr

-- | Parser for Boolean inequality
pBGt :: Parser BExpr
pBGt = try $ do
  t1 <- pAExpr
  _ <- symbol ">"
  Gt t1 <$> pAExpr