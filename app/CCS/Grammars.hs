module CCS.Grammars where

import Data.Set (Set)

-- | AST for labels, i.e., channel names and their complements
data Label
  = Input String
  | Output String
  deriving (Eq, Ord, Show)

-- | AST for actions, i.e., transitions over channel names or internal
data Action
  = ActionName Label
  | Tau
  deriving (Eq, Ord, Show)

-- | AST for processes, i.e., process literals or process operators
data Process
  = ProcessName String
  | ActionPrefix Action Process
  | Sum Process Process
  | Parallel Process Process
  | Relabelling Process (Action -> Action)
  | Restriction Process (Set Label)

instance Show Process where
  show (ProcessName name) = "ProcessName " ++ name
  show (ActionPrefix action process) = "ActionPrefix (" ++ show action ++ ") (" ++ show process ++ ")"
  show (Sum p1 p2) = "Sum (" ++ show p1 ++ ") (" ++ show p2 ++ ")"
  show (Parallel p1 p2) = "Parallel (" ++ show p1 ++ ") (" ++ show p2 ++ ")"
  show (Relabelling process _) = "Relabelling (" ++ show process ++ ") <function>"
  show (Restriction process labels) = "Restriction (" ++ show process ++ ") (" ++ show labels ++ ")"