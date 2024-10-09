module Parser.Token (Token (..)) where

import Data.List (intercalate)
import Data.Set (Set)
import Data.Text (Text, unpack)
import Grammars.AST (AExpr, BExpr, RelabellingFunction)

-- | AST for tokens, used by the token parser
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
  | TBranch BExpr Token Token
  deriving (Eq)

instance Show Token where
  show :: Token -> String
  show token = "'" <> showInternal token <> "' " <> tokenType token
    where
      showInternal :: Token -> String
      showInternal (TProc name []) = unpack name
      showInternal (TProc name vars) = unpack name <> "(" <> intercalate ", " (map show vars) <> ")"
      showInternal (TActIn name Nothing) = unpack name
      showInternal (TActIn name (Just e)) = unpack name <> "(" <> show e <> ")"
      showInternal (TActOut name Nothing) = unpack name
      showInternal (TActOut name (Just e)) = unpack name <> "(" <> show e <> ")"
      showInternal TActTau = "τ"
      showInternal (RelFn f) = show f
      showInternal (ResSet s) = show s
      showInternal (TPre t1 t2) = showWithOp t1 "." t2
      showInternal (TChoice t1 t2) = showWithOp t1 "+" t2
      showInternal (TPar t1 t2) = showWithOp t1 "|" t2
      showInternal (TRes t1 t2) = showWithOp t1 "\\" t2
      showInternal (TRel t1 t2) = showInternal t1 <> showInternal t2
      showInternal (TBranch g t1 t2) = "if " <> show g <> " then " <> showInternal t1 <> " else " <> showInternal t2

      tokenType :: Token -> String
      tokenType (TProc _ _) = "<Process>"
      tokenType (TActIn _ _) = "<Input Action>"
      tokenType (TActOut _ _) = "<Output Action>"
      tokenType TActTau = "<Internal Action>"
      tokenType (RelFn _) = "<Relabelling Function>"
      tokenType (ResSet _) = "<Restriction Set>"
      tokenType (TPre _ _) = "<Action Prefix>"
      tokenType (TChoice _ _) = "<Process Summation>"
      tokenType (TPar _ _) = "<Process Composition>"
      tokenType (TRes _ _) = "<Channel Restriction>"
      tokenType (TRel _ _) = "<Channel Relabeling>"
      tokenType (TBranch _ _ _) = "<Branching>"

      showWithOp :: Token -> String -> Token -> String
      showWithOp t1 op t2 = showInternal t1 <> " " <> op <> " " <> showInternal t2