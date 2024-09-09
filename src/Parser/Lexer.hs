module Parser.Lexer (tokenize, pActOut, pActIn, pTProc) where

import AST (RelabellingFunction (..), RelabellingMapping (..))
import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Data.Functor (($>))
import Data.Set (fromList)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Parser.AExprParser (pAExpr)
import Parser.AST (Token (..))
import Parser.BExprParser (pBExpr)
import Parser.Utils (Parser, binaryL, binaryL', binaryR, comma, curlyParens, lexeme, pWord, roundParens, sc, slash, squareParens, symbol)
import Text.Megaparsec (MonadParsec (eof, try), ParseErrorBundle, choice, many, option, parse, sepBy1, (<?>))
import Text.Megaparsec.Char (char, letterChar, lowerChar, upperChar)

-- | Tokenizes the given text
tokenize :: Text -> Either (ParseErrorBundle Text Void) (Maybe Token)
tokenize = parse (sc *> choice [Just <$> pToken <* eof, eof $> Nothing]) ""

-- | Token parser
pToken :: Parser Token
pToken = makeExprParser pTerm operatorTable

-- | Terms parses for makeExprParser
pTerm :: Parser Token
pTerm = choice [roundParens pToken, pTActTau, pTBranch, pRelFn, pResSet, pActOut, pActIn, pTProc]

-- | Operator table for makeExprParser
operatorTable :: [[Operator Parser Token]]
operatorTable =
  [ [ binaryL "\\" TRes, -- Restriction: A \ {a,b,c}
      binaryL' "[" TRel -- Relabeling: A[a/b,c/d]
    ],
    [binaryR "." TPre], -- Prefixing: a.A
    [binaryL "|" TPar], -- Parallel composition: A | B
    [binaryL "+" TChoice], -- ND Choice: A + B
    [binaryL "=" TAss] -- Assignment: A = b.B
  ]

-- | Parser for process names
pTProc :: Parser Token
pTProc = do
  name <- pProcName <?> "Process Name"
  vars <- option [] (try $ roundParens (pAExpr `sepBy1` comma)) <?> "Process variables"
  if name == "0" && not (null vars)
    then fail "Error: dead process cannot have variables"
    else return $ TProc name vars
  where
    pProcName :: Parser Text
    pProcName = lexeme (choice [(:) <$> upperChar <*> many letterChar, pure <$> char '0']) >>= checkReserved . pack

-- | Parser for input actions
pActIn :: Parser Token
pActIn = do
  name <- pChannel <?> "Input channel name"
  var <- option Nothing (Just <$> roundParens (pAExpr)) <?> "Input action variable"
  return $ TActIn name var

-- | Parser for output actions
pActOut :: Parser Token
pActOut = do
  _ <- char '\'' <?> "\' character, prefixes output channel names"
  name <- pChannel <?> "Output channel name"
  var <- option Nothing (Just <$> (roundParens (pAExpr))) <?> "Input action variable"
  return $ TActOut name var

-- | Parser for channel names
pChannel :: Parser Text
pChannel = lexeme ((:) <$> lowerChar <*> many letterChar >>= (checkReserved . pack)) <?> "Channel name"

-- | Parser for internal actions
pTActTau :: Parser Token
pTActTau = TActTau <$ symbol "τ" <?> "Internal action"

-- | Parser for action relabelling functions
pActionRelabel :: Parser RelabellingMapping
pActionRelabel = RelabellingMapping <$> (pChannel <* slash) <*> pChannel <?> "Relabelling mapping, e.g., a/b"

-- | Parser for relabelling functions
pRelFn :: Parser Token
pRelFn = RelFn . RelabellingFunction <$> (squareParens $ pActionRelabel `sepBy1` comma) <?> "Relabelling function, e.g., [a/b, c/d]"

-- | Parser for channel restriction sets
pResSet :: Parser Token
pResSet = ResSet . fromList <$> (curlyParens $ pChannel `sepBy1` comma) <?> "Restriction set, e.g., {a,b,c}"

-- | Parser for branching
pTBranch :: Parser Token
pTBranch = do
  _ <- lexeme $ pWord "if"
  guard <- pBExpr
  _ <- lexeme $ pWord "then"
  thenBranch <- pToken
  _ <- lexeme $ pWord "else"
  elseBranch <- pToken
  return $ TBranch guard thenBranch elseBranch

-- | List of reserved keywords
reservedKeywords :: [Text]
reservedKeywords = ["if", "then", "else"]

-- | Checks whether the provided word is contained in the reserved keywords list.
-- If it is, then it fails with an error message
-- Otherwise it is wrapped in the Parser monad and returned
checkReserved :: Text -> Parser Text
checkReserved word =
  if word `elem` reservedKeywords
    then fail $ unpack word ++ " is a reserved keyword"
    else return word