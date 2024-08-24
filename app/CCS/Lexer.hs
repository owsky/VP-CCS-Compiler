module CCS.Lexer where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), anySingle, between, choice, manyTill, parse, some)
import Text.Megaparsec.Char (char, letterChar)
import Text.Megaparsec.Error (ParseErrorBundle)
import Utils (Parser, binary, lexeme, symbol)

-- | AST of tokens for lexer
data Token
  = TExpr Text
  | TPre Token Token
  | TSum Token Token
  | TPar Token Token
  | TRes Token Token
  | TRel Token Token
  | TAss Token Token
  deriving (Show)

-- | Parses action names and process names
pName :: Parser Token
pName = TExpr . pack <$> lexeme (choice [some letterChar, (:) <$> char '\'' <*> some letterChar, pure <$> char '0'])

-- | Parses the content between curly braces as text for the restriction operator
pRes :: Parser Token
pRes = do
  void $ symbol "{"
  res <- lexeme $ manyTill anySingle (char '}')
  return $ TExpr $ pack res

-- | Parses the content between square braces as text for the relabelling operator
pRel :: Parser Token
pRel = do
  void $ symbol "["
  rel <- lexeme $ manyTill anySingle (char ']')
  return $ TExpr $ pack rel

-- | Uses the given parser to parse the content enclosed in round braces
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Terms parses for makeExprParser
pTerm :: Parser Token
pTerm = choice [parens pToken, pName, pRes, pRel]

-- | Operator table for makeExprParser
operatorTable :: [[Operator Parser Token]]
operatorTable =
  [ [ binary "\\" TRes, -- Restriction: Expr \ {...}
      binary "[" TRel -- Relabelling: Expr[...]
    ],
    [binary "." TPre], -- Prefixing: Expr.Expr
    [binary "|" TPar], -- Parallel composition: Expr | Expr
    [binary "+" TSum], -- Summation: Expr + Expr
    [binary "=" TAss] -- Assignment: Expr = Expr
  ]

-- | Token parser
pToken :: Parser Token
pToken = makeExprParser pTerm operatorTable

-- | Tokenizes the given text
tokenize :: Text -> Either (ParseErrorBundle Text Void) Token
tokenize = parse (pToken <* eof) "stdin"