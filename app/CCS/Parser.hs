module CCS.Parser where

import CCS.Grammars (Action (..), Label (..), Process (..), RelabellingFunction (..), RelabellingMapping (..), Statement (..), Token (..), getLabelName)
import CCS.Utils (Parser, binaryL, binaryR, binaryR', comma, curlyParens, lexeme, roundParens, sc, slash, squareParens, symbol)
import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Data.Set (Set, fromList)
import Data.Text (Text, pack)
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec (MonadParsec (eof), choice, label, many, parse, sepBy1, (<?>))
import Text.Megaparsec.Char (char, letterChar, lowerChar, upperChar)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)

-- | Parser for process names
pTProc :: Parser Token
pTProc = TProc . pack <$> lexeme (choice [(:) <$> upperChar <*> many letterChar, pure <$> char '0']) <?> "Process name"

-- | Polymorphic parser for input actions
pActIn :: forall a. (Text -> a) -> Parser a
pActIn constr = constr . pack <$> lexeme ((:) <$> lowerChar <*> many letterChar) <?> "Input action name"

-- | Polymorphic parser for output actions
pActOut :: forall a. (Text -> a) -> Parser a
pActOut constr = label "Output action name" $ do
  tickChar <- char '\''
  firstChar <- lowerChar
  restChars <- many letterChar
  return $ constr $ pack (tickChar : firstChar : restChars)

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

-- | Terms parses for makeExprParser
pTerm :: Parser Token
pTerm = choice [roundParens pToken, pRelFn, pResSet, pTActTau, pActOut TActOut, pActIn TActIn, pTProc]

-- | Operator table for makeExprParser
operatorTable :: [[Operator Parser Token]]
operatorTable =
  [ [ binaryR "\\" TRes, -- Restriction: A \ {a,b,c}
      binaryR' "[" TRel -- Relabeling: A[a/b,c/d]
    ],
    [binaryR "." TPre], -- Prefixing: a.A
    [binaryL "|" TPar], -- Parallel composition: A | B
    [binaryL "+" TSum], -- Summation: A + B
    [binaryL "=" TAss] -- Assignment: A = b.B
  ]

-- | Token parser
pToken :: Parser Token
pToken = makeExprParser pTerm operatorTable

-- | Tokenizes the given text
tokenize :: Text -> Either (ParseErrorBundle Text Void) Token
tokenize = parse (sc *> pToken <* eof) ""

-- | Attempts to convert a given token into an action
tokenToAction :: Token -> Either String Action
tokenToAction (TActIn name) = Right $ ActionName $ Input name
tokenToAction (TActOut name) = Right $ ActionName $ Output name
tokenToAction TActTau = Right Tau
tokenToAction other = Left $ "Expecting action, got something else: " ++ show other

-- | Attempts to convert a given into into a relabelling function
tokenToRelabelFn :: Token -> Either String RelabellingFunction
tokenToRelabelFn token = case token of
  RelFn f -> Right f
  _ -> Left "Error while parsing the relabeling function"

-- | Attempts to convert a given into into a label set for restriction
tokenToLabelSet :: Token -> Either String (Set Label)
tokenToLabelSet token = case token of
  ResSet s -> Right s
  _ -> Left "Error while parsing the restriction set"

-- | Attempts to convert a given into into a process
tokenToProcess :: Token -> Either String Process
tokenToProcess (TProc name) = Right $ ProcessName name
tokenToProcess (TPre left right) = do
  action <- tokenToAction left
  process <- tokenToProcess right
  return $ ActionPrefix action process
tokenToProcess (TSum left right) = do
  p1 <- tokenToProcess left
  p2 <- tokenToProcess right
  return $ Sum p1 p2
tokenToProcess (TPar left right) = do
  p1 <- tokenToProcess left
  p2 <- tokenToProcess right
  return $ Parallel p1 p2
tokenToProcess (TRel left right) = do
  process <- tokenToProcess left
  relabelFn <- tokenToRelabelFn right
  return $ Relabelling process relabelFn
tokenToProcess (TRes left right) = do
  process <- tokenToProcess left
  labelSet <- tokenToLabelSet right
  return $ Restriction process labelSet
tokenToProcess token = Left $ "Expected a process, got: " ++ show token

-- | Attempts to convert a given into into a statement
tokenToStatement :: Token -> Either String Statement
tokenToStatement (TAss left right) = do
  lhs <- tokenToProcess left
  rhs <- tokenToProcess right
  case lhs of
    ProcessName name -> return $ Assignment name rhs
    _ -> Left $ "Expected a process name on the left-hand side of the assignment, got: " ++ show rhs
tokenToStatement token = Left $ "Expecting a statement, got: " ++ show token

-- | Attempts to parse the given text into a statement
parseInput :: Text -> Either String Statement
parseInput l = do
  let toks = tokenize l
  case toks of
    Left err -> Left $ errorBundlePretty err
    Right tokens -> do
      trace ("Debug - Token: " ++ show tokens) $ do
        tokenToStatement tokens