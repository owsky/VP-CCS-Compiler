module Parser.AExprParser (pAExpr, pAVar) where

import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Data.Text (pack)
import Grammars.AST (AExpr (..))
import Parser.Utils (Parser, binaryL, decimal, lexeme, roundParens)
import Text.Megaparsec (MonadParsec (hidden), choice, some, (<?>))
import Text.Megaparsec.Char (lowerChar)

-- | Arithmetic expressions parser
pAExpr :: Parser AExpr
pAExpr = makeExprParser pTerm operatorTable

pTerm :: Parser AExpr
pTerm = choice [hidden $ roundParens pAExpr, pAVal, pAVar]

operatorTable :: [[Operator Parser AExpr]]
operatorTable =
  [ [binaryL "*" Mul],
    [ binaryL "+" Sum,
      binaryL "-" Min
    ]
  ]

-- | Arithmetic value parser
pAVal :: Parser AExpr
pAVal = AVal <$> decimal <?> "arithmetic value"

-- | Arithmetic variable parser
pAVar :: Parser AExpr
pAVar = AVar . pack <$> lexeme (some $ lowerChar) <?> "arithmetic variable"