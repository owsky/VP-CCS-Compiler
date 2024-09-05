module CCS.Grammars where

import Data.Set (Set)
import Data.Text (Text, unpack)

-- | AST for labels, i.e., channel names and their complements
data Label
  = Input Text
  | Output Text
  deriving (Eq, Ord)

instance Show Label where
  show :: Label -> String
  show (Input name) = unpack name
  show (Output name) = unpack name

-- | AST for actions, i.e., transitions over channel names or internal
data Action
  = ActionName Label
  | Tau
  deriving (Eq)

instance Show Action where
  show :: Action -> String
  show (ActionName label) = show label
  show Tau = "τ"

-- | AST for relabellings, e.g., a/b
data RelabellingMapping = RelabellingMapping Text Text deriving (Show, Eq)

-- | AST for relabelling functions, e.g., [a/b, c/d]
newtype RelabellingFunction = RelabellingFunction [RelabellingMapping] deriving (Show, Eq)

-- | AST for processes, i.e., process literals or process operators
data Process
  = ProcessName Text
  | ActionPrefix Action Process
  | Choice Process Process
  | Parallel Process Process
  | Relabelling Process RelabellingFunction
  | Restriction Process (Set Label)
  deriving (Eq)

instance Show Process where
  show :: Process -> String
  show (ProcessName name) = unpack name
  show (ActionPrefix act proc) = case proc of
    (Choice p1 p2) -> show act ++ "." ++ "(" ++ show (Choice p1 p2) ++ ")"
    (Parallel p1 p2) -> show act ++ "." ++ "(" ++ show (Parallel p1 p2) ++ ")"
    _ -> show act ++ "." ++ show proc
  show (Choice p1 p2) = case p2 of
    (ProcessName "0") -> show p1
    _ -> show p1 ++ " + " ++ show p2
  show (Parallel p1 p2) = show p1 ++ " | " ++ show p2
  show (Relabelling p fn) = show p ++ show fn
  show (Restriction p s) = show p ++ show s

-- | AST for statements
data Statement
  = Assignment Process Process
  deriving (Eq)

instance Show Statement where
  show :: Statement -> String
  show (Assignment p1 p2) = show p1 ++ " = " ++ show p2