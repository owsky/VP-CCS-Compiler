module CCS.Parser where

import CCS.Grammars (Action (..), Label (..), Process (..), RelabellingFunction (..), RelabellingMapping (..), Token (..), getLabelName)
import CCS.Utils (Parser, binary, binary', comma, curlyParens, lexeme, roundParens, sc, slash, squareParens, symbol)
import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Data.Set (Set, fromList)
import Data.Text (Text, pack)
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec (MonadParsec (eof), choice, label, parse, sepBy1, some, (<?>))
import Text.Megaparsec.Char (char, letterChar)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)

pTokId :: Parser Token
pTokId = TokId . pack <$> lexeme (choice [some letterChar, (:) <$> char '\'' <*> some letterChar, pure <$> char '0']) <?> "Token identifier"

pLabel :: Parser Label
pLabel =
  choice
    [ Input . pack <$> lexeme (some letterChar),
      Output . pack <$> lexeme ((:) <$> char '\'' <*> some letterChar)
    ]
    <?> "Channel name"

-- | Parser for action relabelling functions
pActionRelabel :: Parser RelabellingMapping
pActionRelabel = label "Relabelling mapping, e.g., a/b" $ do
  ogLabel <- pLabel
  _ <- slash
  RelabellingMapping (getLabelName ogLabel) . getLabelName <$> pLabel

pRelFn :: Parser Token
pRelFn = label "Relabelling function, e.g., [a/b, c/d]" $ do
  mappings <- squareParens $ pActionRelabel `sepBy1` comma
  return (RelFn $ RelabellingFunction mappings)

pResSet :: Parser Token
pResSet = label "Restriction set, e.g., {a,b,c}" $ do
  labels <- curlyParens $ pLabel `sepBy1` comma
  return $ ResSet (fromList labels)

-- | Terms parses for makeExprParser
pTerm :: Parser Token
pTerm = choice [roundParens pToken, pRelFn, pResSet, pTokId]

-- | Operator table for makeExprParser
operatorTable :: [[Operator Parser Token]]
operatorTable =
  [ [ binary "\\" TRes, -- Restriction: A \ {a,b,c}
      binary' "[" TRel -- Relabeling: A[a/b,c/d]
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

pAction :: Parser Action
pAction = choice [Tau <$ symbol "τ" <?> "Tau action", ActionName <$> lexeme pLabel <?> "Channel name"] <?> "Channel"

tokenToAction :: Token -> Either String Action
tokenToAction token = case token of
  TokId text -> do
    let action = parse (pAction <* eof) "" text
    case action of
      Left _ -> Left "Error while parsing action"
      Right a -> Right a
  _ -> Left "Expecting action, got something else"

tokenToRelabelFn :: Token -> Either String RelabellingFunction
tokenToRelabelFn token = case token of
  RelFn f -> Right f
  _ -> Left "Error while parsing the relabeling function"

tokenToLabelSet :: Token -> Either String (Set Label)
tokenToLabelSet token = case token of
  ResSet s -> Right s
  _ -> Left "Error while parsing the restriction set"

tokenToProcess :: Token -> Either String Process
tokenToProcess (TokId text) = Right $ ProcessName text
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
tokenToProcess (TAss left right) = do
  lhs <- tokenToProcess left
  rhs <- tokenToProcess right
  case lhs of
    ProcessName name -> Right $ Assignment name rhs
    _ -> Left "You can only assign to process names"
tokenToProcess _ = Left "Something wrong occurred"

parseInput :: Text -> Either String Process
parseInput l = do
  let toks = tokenize l
  case toks of
    Left err -> Left $ errorBundlePretty err
    Right tokens -> do
      trace ("Debug - Token: " ++ show tokens) $ do
        tokenToProcess tokens