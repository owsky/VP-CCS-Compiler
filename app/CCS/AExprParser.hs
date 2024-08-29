module CCS.AExprParser where

import CCS.Grammars (AExpr (..))
import CCS.Utils (Parser, binaryL, lexeme, roundParens, sc, unsignedInteger)
import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), ParseErrorBundle, choice, parse, some, (<?>))
import Text.Megaparsec.Char (letterChar)

parseArithmeticExpression :: Text -> Either (ParseErrorBundle Text Void) AExpr
parseArithmeticExpression = parse (sc *> pAExpr <* eof) ""

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
pAVal = AVal <$> unsignedInteger <?> "Arithmetic value"

pAVar :: Parser AExpr
pAVar = AVar . pack <$> lexeme (some letterChar) <?> "Arithmetic variables"