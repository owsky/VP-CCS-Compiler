module CCS.Grammars where

import Data.Set (Set)
import Data.Text (Text)

-- | AST for labels, i.e., channel names and their complements
data Label
  = Input Text
  | Output Text
  deriving (Eq, Ord, Show)

-- | AST for actions, i.e., transitions over channel names or internal
data Action
  = ActionName Label
  | Tau
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-- | AST for statements
data Statement
  = Assignment Process Process
  deriving (Eq, Show)