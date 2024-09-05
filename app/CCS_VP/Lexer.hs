module CCS_VP.Lexer where

import CCS_VP.AExprParser (pAExpr)
import CCS_VP.BExprParser (pBExpr)
import CCS_VP.Grammars (Label (..), RelabellingFunction (..), RelabellingMapping (..), Token (..), getLabelName)
import CCS_VP.Utils (Parser, binaryL, binaryR, binaryR', comma, curlyParens, lexeme, roundParens, sc, slash, squareParens, symbol, textUntil)
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Data.Functor (($>))
import Data.Set (fromList)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, try), ParseErrorBundle, choice, label, many, parse, sepBy1, (<?>))
import Text.Megaparsec.Char (char, letterChar, lowerChar, string, upperChar)
import Text.Megaparsec.Error (errorBundlePretty)

-- | Tokenizes the given text
tokenize :: Text -> Either (ParseErrorBundle Text Void) (Maybe Token)
tokenize = parse tryPToken ""

tryPToken :: Parser (Maybe Token)
tryPToken = do
  void sc
  choice
    [ Just <$> try pToken,
      eof $> Nothing
    ]

-- pTArith :: Parser Token
-- pTArith = TArith <$> pAExpr

-- pTBool :: Parser Token
-- pTBool = TBool <$> pBExpr

-- | Token parser
pToken :: Parser Token
pToken = makeExprParser pTerm operatorTable

-- | Terms parses for makeExprParser
pTerm :: Parser Token
pTerm = choice [roundParens pToken, pTBranch, pRelFn, pResSet, pTActTau, pActOutV, pActOut TActOut, pActInV, pActIn TActIn, pTProcV, pTProc]

-- | Operator table for makeExprParser
operatorTable :: [[Operator Parser Token]]
operatorTable =
  [ [ binaryR "\\" TRes, -- Restriction: A \ {a,b,c}
      binaryR' "[" TRel -- Relabeling: A[a/b,c/d]
    ],
    [binaryR "." TPre], -- Prefixing: a.A
    [binaryL "|" TPar], -- Parallel composition: A | B
    [binaryL "+" TChoice], -- ND Choice: A + B
    [binaryL "=" TAss] -- Assignment: A = b.B
  ]

-- | Parser for process names
pTProc :: Parser Token
pTProc = try (TProc . pack <$> lexeme (choice [(:) <$> upperChar <*> many letterChar, pure <$> char '0'])) <?> "Process name"

pTProcV :: Parser Token
pTProcV = try $ do
  pName <- (:) <$> upperChar <*> many letterChar
  v <- roundParens (pAExpr `sepBy1` comma)
  return $ TProcV (pack pName) (TArith v)

-- | Polymorphic parser for input actions
pActIn :: forall a. (Text -> a) -> Parser a
pActIn constr = constr . pack <$> lexeme ((:) <$> lowerChar <*> many letterChar) <?> "Input action name"

pActInV :: Parser Token
pActInV = try $ do
  aName <- (:) <$> lowerChar <*> many letterChar
  v <- roundParens (pAExpr `sepBy1` comma)
  return $ TActInV (pack aName) (TArith v)

-- | Polymorphic parser for output actions
pActOut :: forall a. (Text -> a) -> Parser a
pActOut constr = label "Output action name" $ do
  _ <- char '\''
  aName <- (:) <$> lowerChar <*> many letterChar
  return $ constr $ pack ("'" ++ aName)

pActOutV :: Parser Token
pActOutV = do
  _ <- char '\''
  aName <- (:) <$> lowerChar <*> many letterChar
  v <- roundParens (pAExpr `sepBy1` comma)
  return $ TActOutV (pack $ "'" ++ aName) (TArith v)

-- | Parser for implicit actions
pTActTau :: Parser Token
pTActTau = TActTau <$ symbol "τ" <?> "Implicit action"

-- | Parser for labels, i.e., used for named transitions
pLabel :: Parser Label
pLabel =
  choice
    [ pActIn Input,
      pActOut Output
    ]
    <?> "Channel name"

-- | Parser for action relabelling functions
pActionRelabel :: Parser RelabellingMapping
pActionRelabel = label "Relabelling mapping, e.g., a/b" $ do
  ogLabel <- pLabel
  _ <- slash
  RelabellingMapping (getLabelName ogLabel) . getLabelName <$> pLabel

-- | Parser for relabelling functions
pRelFn :: Parser Token
pRelFn = label "Relabelling function, e.g., [a/b, c/d]" $ do
  mappings <- squareParens $ pActionRelabel `sepBy1` comma
  return (RelFn $ RelabellingFunction mappings)

-- | Parser for channel restriction sets
pResSet :: Parser Token
pResSet = label "Restriction set, e.g., {a,b,c}" $ do
  labels <- curlyParens $ pLabel `sepBy1` comma
  return $ ResSet (fromList labels)

-- | Parser for branching
pTBranch :: Parser Token
pTBranch = do
  _ <- lexeme $ string "if"
  bexpText <- textUntil (lexeme $ string "then")
  thenBranchText <- textUntil (lexeme $ string "else")
  elseBranch <- pToken

  -- Parse the text segments using `pToken`
  let bexp = parse pBExpr "" bexpText
  let thenBranch = parse pToken "" thenBranchText

  case (bexp, thenBranch) of
    (Right bexpToken, Right thenBranchToken) ->
      return $ TBranch (TBool bexpToken) thenBranchToken elseBranch
    (Left err, _) -> fail $ errorBundlePretty err
    (_, Left err) -> fail $ errorBundlePretty err