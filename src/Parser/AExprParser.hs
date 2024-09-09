module Parser.AExprParser (pAExpr) where

import AST (AExpr (..))
import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Data.Text (pack)
import Parser.Utils (Parser, binaryL, decimal, lexeme, roundParens)
import Text.Megaparsec (choice, some, (<?>))
import Text.Megaparsec.Char (letterChar)

pAExpr :: Parser AExpr
pAExpr = makeExprParser pTerm operatorTable

pTerm :: Parser AExpr
pTerm = choice [roundParens pAExpr, pAVal, pAVar]

operatorTable :: [[Operator Parser AExpr]]
operatorTable =
  [ [binaryL "*" Mul],
    [ binaryL "+" Sum,
      binaryL "-" Min
    ]
  ]

pAVal :: Parser AExpr
pAVal = AVal <$> decimal <?> "Arithmetic value"

pAVar :: Parser AExpr
pAVar = AVar . pack <$> lexeme (some letterChar) <?> "Arithmetic variables"