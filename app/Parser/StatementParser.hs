module Parser.StatementParser (parseInput) where

import AST (Action (..), Label (..), Process (..), RelabellingFunction (..), Statement (..))
import Data.Set (Set)
import Data.Text (Text)
import Debug.Trace (trace)
import Parser.AST (Token (..))
import Parser.Lexer (tokenize)
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
    ProcessName name mvar -> return $ Assignment (ProcessName name mvar) rhs
    _ -> Left $ "Expected a process name on the left-hand side of the assignment, got: " ++ show rhs
tokenToStatement token = Left $ "Expecting a statement, got: " ++ show token

-- | Attempts to convert a given token into an action
tokenToAction :: Token -> Either String Action
tokenToAction (TActIn name) = Right $ ActionName (Input name) Nothing
tokenToAction (TActInV name expr) = Right $ ActionName (Input name) (Just expr)
tokenToAction (TActOut name) = Right $ ActionName (Output name) Nothing
tokenToAction (TActOutV name expr) = Right $ ActionName (Output name) (Just expr)
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
tokenToProcess (TProc name) = Right $ ProcessName name []
tokenToProcess (TProcV name exprs) = Right $ ProcessName name exprs
tokenToProcess (TPre left right) = ActionPrefix <$> tokenToAction left <*> tokenToProcess right
tokenToProcess (TChoice left right) = Choice <$> tokenToProcess left <*> tokenToProcess right
tokenToProcess (TPar left right) = Parallel <$> tokenToProcess left <*> tokenToProcess right
tokenToProcess (TRel left right) = Relabelling <$> tokenToProcess left <*> tokenToRelabelFn right
tokenToProcess (TRes left right) = Restriction <$> tokenToProcess left <*> tokenToLabelSet right
tokenToProcess (TBranch guard t1 t2) = IfThenElse guard <$> tokenToProcess t1 <*> tokenToProcess t2
tokenToProcess token = Left $ "Expected a process, got: " ++ show token