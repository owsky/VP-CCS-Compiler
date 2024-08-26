module CCS.Lexer.Lexer where

import CCS.Lexer.Utils (Parser, binary, comma, curlyParens, lexeme, roundParens, sc, slash)
import CCS.Parser.Grammars (Label (..), RelabellingFunction (..), RelabellingMapping (..), getLabelName)
import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Data.Set (Set, fromList)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, try), choice, parse, sepBy1, some, (<|>))
import Text.Megaparsec.Char (char, letterChar)
import Text.Megaparsec.Error (ParseErrorBundle)

-- | AST of tokens for lexer
data Token
  = TokId Text
  | RelFn RelabellingFunction
  | ResSet (Set Label)
  | TPre Token Token
  | TSum Token Token
  | TPar Token Token
  | TRes Token Token
  | TRel Token Token
  | TAss Token Token
  deriving (Eq, Show)

pTokId :: Parser Token
pTokId = TokId . pack <$> lexeme (choice [some letterChar, (:) <$> char '\'' <*> some letterChar, pure <$> char '0'])

pLabelInput :: Parser Label
pLabelInput = Input . pack <$> lexeme (some letterChar)

pLabelOutput :: Parser Label
pLabelOutput = Output . pack <$> lexeme ((:) <$> char '\'' <*> some letterChar)

pLabel :: Parser Label
pLabel = choice [pLabelOutput, pLabelInput]

-- | Parser for action relabelling functions
pActionRelabel :: Parser RelabellingMapping
pActionRelabel = do
  ogLabel <- pLabel
  _ <- slash
  newLabel <- pLabel
  return RelabellingMapping {from = getLabelName ogLabel, to = getLabelName newLabel}

pRelFn :: Parser Token
pRelFn = try $ do
  m <- pActionRelabel `sepBy1` comma
  _ <- char ']' <|> fail "Expected closing square bracket ']'"
  return $ RelFn (RelabellingFunction {mappings = m})

pResSet :: Parser Token
pResSet = do
  labels <- curlyParens $ pLabel `sepBy1` comma
  return $ ResSet (fromList labels)

-- | Terms parses for makeExprParser
pTerm :: Parser Token
pTerm = choice [roundParens pToken, pRelFn, pTokId, pResSet]

-- | Operator table for makeExprParser
operatorTable :: [[Operator Parser Token]]
operatorTable =
  [ [ binary "\\" TRes, -- Restriction: A \ {a,b,c}
      binary "[" TRel -- Relabeling: A[a/b,c/d]
    ],
    [binary "." TPre], -- Prefixing: a.A
    [binary "|" TPar], -- Parallel composition: A | B
    [binary "+" TSum], -- Summation: A + B
    [binary "=" TAss] -- Assignment: A = b.B
  ]

-- | Token parser
pToken :: Parser Token
pToken = makeExprParser pTerm operatorTable

-- | Tokenizes the given text
tokenize :: Text -> Either (ParseErrorBundle Text Void) Token
tokenize = parse (sc *> pToken <* eof) ""