module Parser.StatementParser (parseStatement) where

import Control.Monad (void)
import Data.Set (Set)
import Data.Text (Text)
import Grammars.AST (Label (..), RelabellingFunction (..))
import Grammars.Pure_AST as Pure (Statement (..))
import Grammars.VP_AST as VP (Action (..), Process (..), Statement (..))
import Parser.Token (Token (..))
import Parser.TokenParser (pToken)
import Parser.Utils (Parser, eitherToMonad, sc', symbol)
import Text.Megaparsec (MonadParsec (eof, label), (<|>))
import Text.Megaparsec.Char (eol)
import Translator.Translate (translateStatement)

parseStatement :: Int -> Parser [Pure.Statement]
parseStatement maxInt = sc' *> pStatement maxInt <* (void eol <|> eof)

-- | Statement parser
pStatement :: Int -> Parser [Pure.Statement]
pStatement maxInt = label "statement" $ do
  lhs <- pToken
  lhsP <- eitherToMonad $ tokenToProcess lhs
  _ <- symbol "="
  rhs <- pToken
  rhsP <- eitherToMonad $ tokenToProcess rhs
  eitherToMonad $ translateStatement maxInt (VP.Assignment lhsP rhsP)

-- | Attempts to convert a given token into an action
tokenToAction :: Token -> Either String Action
tokenToAction (TActIn name expr) = Right $ ActionName (Input name) expr
tokenToAction (TActOut name expr) = Right $ ActionName (Output name) expr
tokenToAction TActTau = Right $ Tau
tokenToAction other = Left $ "unexpected " <> show other <> "\nexpecting <Action>"

-- | Attempts to convert a given into into a relabelling function
tokenToRelabelFn :: Token -> Either String RelabellingFunction
tokenToRelabelFn (RelFn f) = Right f
tokenToRelabelFn other = Left $ "unexpected " <> show other <> "\nexpecting <Relabeling Function>"

-- | Attempts to convert a given into into a label set for restriction
tokenToLabelSet :: Token -> Either String (Set Text)
tokenToLabelSet (ResSet s) = Right s
tokenToLabelSet other = Left $ "unexpected " <> show other <> "\nexpecting <Restriction Set>"

-- | Attempts to convert a given into into a process
tokenToProcess :: Token -> Either String Process
tokenToProcess (TProc name exprs) = Right $ ProcessName name exprs
tokenToProcess (TPre left right) = ActionPrefix <$> tokenToAction left <*> tokenToProcess right
tokenToProcess (TChoice left right) = Choice <$> tokenToProcess left <*> tokenToProcess right
tokenToProcess (TPar left right) = Parallel <$> tokenToProcess left <*> tokenToProcess right
tokenToProcess (TRel left right) = Relabelling <$> tokenToProcess left <*> tokenToRelabelFn right
tokenToProcess (TRes left right) = Restriction <$> tokenToProcess left <*> tokenToLabelSet right
tokenToProcess (TBranch guard t1 t2) = IfThenElse guard <$> tokenToProcess t1 <*> tokenToProcess t2
tokenToProcess token = Left $ "unexpected " <> show token <> "\nexpecting <Process>"