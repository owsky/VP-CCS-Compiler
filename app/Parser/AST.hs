module Parser.AST (Token (..)) where

import AST (AExpr, BExpr, Label, RelabellingFunction)
import Data.Set (Set)
import Data.Text (Text)

-- | AST for tokens, used by the parser
data Token
  = TArith [AExpr]
  | TBool BExpr
  | TProc Text
  | TProcV Text Token
  | TActIn Text
  | TActInV Text Token
  | TActOut Text
  | TActOutV Text Token
  | TActTau
  | RelFn RelabellingFunction
  | ResSet (Set Label)
  | TPre Token Token
  | TChoice Token Token
  | TPar Token Token
  | TRes Token Token
  | TRel Token Token
  | TAss Token Token
  | TBranch Token Token Token
  deriving (Eq, Show)