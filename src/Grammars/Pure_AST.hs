module Grammars.Pure_AST where

import Data.List (intercalate, sort)
import Data.Set (Set, toList)
import Data.Text (Text, unpack)
import Grammars.AST (AExpr, BExpr, Label, RelabellingFunction)

-- | AST for actions, i.e., transitions over channel names or internal
data Action
  = ActionName Label (Maybe AExpr)
  | Tau
  deriving (Eq)

instance Show Action where
  show :: Action -> String
  show (ActionName label Nothing) = show label
  show (ActionName label (Just expr)) = show label ++ "(" ++ show expr ++ ")"
  show Tau = "τ"

-- | AST for processes, i.e., process literals or process operators
data Process
  = ProcessName Text [AExpr]
  | ActionPrefix Action Process
  | Choice Process Process
  | Parallel Process Process
  | Relabelling Process RelabellingFunction
  | Restriction Process (Set Text)
  | IfThenElse BExpr Process Process
  deriving (Eq)

instance Show Process where
  show :: Process -> String
  show (ProcessName name []) = unpack name
  show (ProcessName name vars) = unpack name ++ "(" ++ (intercalate ", " (map show vars)) ++ ")"
  show (ActionPrefix act proc) = case proc of
    (Choice p1 p2) -> show act ++ "." ++ "(" ++ show (Choice p1 p2) ++ ")"
    (Parallel p1 p2) -> show act ++ "." ++ "(" ++ show (Parallel p1 p2) ++ ")"
    _ -> show act ++ "." ++ show proc
  show (Choice p1 p2) = case p2 of
    (ProcessName "0" []) -> show p1
    (ProcessName "0" _) -> error "A dead process cannot have variables"
    _ -> show p1 ++ " + " ++ show p2
  show (Parallel p1 p2) = show p1 ++ " | " ++ show p2
  show (Relabelling p fn) = show p ++ show fn
  show (Restriction p s) = show p ++ " \\ {" ++ intercalate ", " (map unpack (sort (toList s))) ++ "}"
  show (IfThenElse guard p1 p2) = "if " ++ show guard ++ " then " ++ show p1 ++ " else " ++ show p2

-- | AST for statements
data Statement
  = Assignment Process Process
  deriving (Eq)

instance Show Statement where
  show :: Statement -> String
  show (Assignment p1 p2) = show p1 ++ " = " ++ show p2