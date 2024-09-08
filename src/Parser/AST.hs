module Parser.AST (Token (..)) where

import AST (AExpr, BExpr, RelabellingFunction)
import Data.Set (Set)
import Data.Text (Text)

-- | AST for tokens, used by the parser
data Token
  = TProc Text [AExpr]
  | TActIn Text (Maybe AExpr)
  | TActOut Text (Maybe AExpr)
  | TActTau
  | RelFn RelabellingFunction
  | ResSet (Set Text)
  | TPre Token Token
  | TChoice Token Token
  | TPar Token Token
  | TRes Token Token
  | TRel Token Token
  | TAss Token Token
  | TBranch BExpr Token Token
  deriving (Eq, Show)