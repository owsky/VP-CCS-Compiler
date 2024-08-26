module CCS.Lexer.Lexer where

import CCS.Lexer.Utils (Parser, binary, lexeme, sc, squareParens, symbol)
import Control.Monad.Combinators.Expr (Operator (InfixN), makeExprParser)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, try), anySingle, between, choice, manyTill_, parse, some)
import Text.Megaparsec.Char (char, letterChar)
import Text.Megaparsec.Debug (dbg)
import Text.Megaparsec.Error (ParseErrorBundle)

-- | AST of tokens for lexer
data Token
  = TExpr Text
  | TPre Token Token
  | TSum Token Token
  | TPar Token Token
  | TRes Token Token
  | TRel Token Token
  | TAss Token Token
  deriving (Eq, Ord, Show)

-- | Parses action names and process names
pName :: Parser Token
pName = dbg "pName" $ TExpr . pack <$> lexeme (choice [some letterChar, (:) <$> char '\'' <*> some letterChar, pure <$> char '0'])

-- | Parses the content between curly braces as text for the restriction operator
pRes :: Parser Token
pRes = try $ do
  (res, brace) <- lexeme $ manyTill_ anySingle (char '}')
  return $ TExpr $ pack (res ++ [brace])

-- | Parses the content between square braces as text for the relabelling operator
pRel :: Parser Token
pRel = dbg "pRel" $ do
  rel <- lexeme $ squareParens (some (choice [symbol ",", symbol "/", pack . pure <$> letterChar]))
  return $ TExpr $ pack ("[" ++ unpack (mconcat rel) ++ "]")

-- | Uses the given parser to parse the content enclosed in round braces
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Terms parses for makeExprParser
pTerm :: Parser Token
pTerm = dbg "pTerm" $ choice [parens pToken, pName]

-- | Operator table for makeExprParser
operatorTable :: [[Operator Parser Token]]
operatorTable =
  [ [ binary "\\" TRes, -- Restriction: Expr \ {...}
      InfixN relabelOperator -- Relabelling: Expr[...]
    ],
    [binary "." TPre], -- Prefixing: Expr.Expr
    [binary "|" TPar], -- Parallel composition: Expr | Expr
    [binary "+" TSum], -- Summation: Expr + Expr
    [binary "=" TAss] -- Assignment: Expr = Expr
  ]

relabelOperator :: Parser (Token -> Token -> Token)
relabelOperator = do
  innerToken <- pRel
  return (\a _ -> TRel a innerToken)

-- | Token parser
pToken :: Parser Token
pToken = makeExprParser pTerm operatorTable

-- | Tokenizes the given text
tokenize :: Text -> Either (ParseErrorBundle Text Void) Token
tokenize = parse (sc *> pToken <* eof) ""