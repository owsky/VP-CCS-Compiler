module CCS.StatementParser where

import CCS.Grammars (Action (..), Label (..), Process (..), RelabellingFunction (..), Statement (..), Token (..))
import CCS.Lexer (tokenize)
import Data.Set (Set)
import Data.Text (Text)
import Debug.Trace (trace)
import Text.Megaparsec (errorBundlePretty)

-- | Attempts to parse the given text into a statement
parseInput :: Text -> Either String (Maybe Statement)
parseInput l = do
  let eTok = tokenize l
  case eTok of
    Left err -> Left $ errorBundlePretty err
    Right mTok -> do
      case mTok of
        Just tok -> do
          trace ("Debug - Token: " ++ show tok) $ do
            statement <- tokenToStatement tok
            return $ Just statement
        Nothing -> return Nothing

-- | Attempts to convert a given into into a statement
tokenToStatement :: Token -> Either String Statement
tokenToStatement (TAss left right) = do
  lhs <- tokenToProcess left
  rhs <- tokenToProcess right
  case lhs of
    ProcessName name -> return $ Assignment name rhs
    _ -> Left $ "Expected a process name on the left-hand side of the assignment, got: " ++ show rhs
tokenToStatement token = Left $ "Expecting a statement, got: " ++ show token

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
tokenToProcess (TChoice left right) = do
  p1 <- tokenToProcess left
  p2 <- tokenToProcess right
  return $ Choice p1 p2
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