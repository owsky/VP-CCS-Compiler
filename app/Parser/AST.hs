module Parser.AST (Token (..)) where

import AST (AExpr, BExpr, Label, RelabellingFunction)
import Data.Set (Set)
import Data.Text (Text)

-- | AST for tokens, used by the parser
-- TODO merge V cases, optimize action in/out parsing
data Token
  = TProc Text
  | TProcV Text [AExpr]
  | TActIn Text
  | TActInV Text AExpr
  | TActOut Text
  | TActOutV Text AExpr
  | TActTau
  | RelFn RelabellingFunction
  | ResSet (Set Label)
  | TPre Token Token
  | TChoice Token Token
  | TPar Token Token
  | TRes Token Token
  | TRel Token Token
  | TAss Token Token
  | TBranch BExpr Token Token
  deriving (Eq, Show)