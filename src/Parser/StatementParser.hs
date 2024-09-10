module Parser.StatementParser (parseInput) where

import AST (Action (..), Label (..), Process (..), RelabellingFunction (..), Statement (..))
import Data.Functor (($>))
import Data.Set (Set)
import Data.Text (Text)
import Parser.AST (Token (..))
import Parser.ProcessParser (pToken)
import Parser.Utils (Parser, sc, symbol)
import Text.Megaparsec (MonadParsec (eof, label), choice, errorBundlePretty, parse)

-- | Attempts to parse the given text into a statement
parseInput :: Text -> Either String (Maybe Statement)
parseInput input = case parse (sc *> choice [Just <$> pStatement <* eof, eof $> Nothing]) "" input of
  Left bundle -> Left $ errorBundlePretty bundle
  Right mStatement -> Right mStatement

-- | Statement parser
pStatement :: Parser Statement
pStatement = label "statement" $ do
  lhs <- pToken
  _ <- symbol "="
  rhs <- pToken
  case (tokenToProcess lhs, tokenToProcess rhs) of
    (Right p1, Right p2) -> return $ Assignment p1 p2
    (Left err, _) -> fail err
    (_, Left err) -> fail err

-- | Attempts to convert a given token into an action
tokenToAction :: Token -> Either String Action
tokenToAction (TActIn name expr) = Right $ ActionName (Input name) expr
tokenToAction (TActOut name expr) = Right $ ActionName (Output name) expr
tokenToAction TActTau = Right Tau
tokenToAction other = Left $ "Expecting action, got something else: " ++ show other

-- | Attempts to convert a given into into a relabelling function
tokenToRelabelFn :: Token -> Either String RelabellingFunction
tokenToRelabelFn token = case token of
  RelFn f -> Right f
  _ -> Left "Error while parsing the relabeling function"

-- | Attempts to convert a given into into a label set for restriction
tokenToLabelSet :: Token -> Either String (Set Text)
tokenToLabelSet token = case token of
  ResSet s -> Right s
  _ -> Left "Error while parsing the restriction set"

-- | Attempts to convert a given into into a process
tokenToProcess :: Token -> Either String Process
tokenToProcess (TProc name exprs) = Right $ ProcessName name exprs
tokenToProcess (TPre left right) = ActionPrefix <$> tokenToAction left <*> tokenToProcess right
tokenToProcess (TChoice left right) = Choice <$> tokenToProcess left <*> tokenToProcess right
tokenToProcess (TPar left right) = Parallel <$> tokenToProcess left <*> tokenToProcess right
tokenToProcess (TRel left right) = Relabelling <$> tokenToProcess left <*> tokenToRelabelFn right
tokenToProcess (TRes left right) = Restriction <$> tokenToProcess left <*> tokenToLabelSet right
tokenToProcess (TBranch guard t1 t2) = IfThenElse guard <$> tokenToProcess t1 <*> tokenToProcess t2
tokenToProcess token = Left $ "Expected a process, got: " ++ show token