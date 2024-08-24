module CCS.Parser.Grammars where

import Data.Set (Set)

-- | AST for labels, i.e., channel names and their complements
data Label
  = Input String
  | Output String
  deriving (Eq, Ord, Show)

getLabelName :: Label -> String
getLabelName (Input str) = str
getLabelName (Output str) = str

data RelabellingMapping = RelabellingMapping
  { from :: String,
    to :: String
  }
  deriving (Show, Eq)

newtype RelabellingFunction = RelabellingFunction
  { mappings :: [RelabellingMapping]
  }
  deriving (Show, Eq)

-- | AST for actions, i.e., transitions over channel names or internal
data Action
  = ActionName Label
  | Tau
  deriving (Eq, Show)

-- | AST for processes, i.e., process literals or process operators
data Process
  = ProcessName String
  | ActionPrefix Action Process
  | Sum Process Process
  | Parallel Process Process
  | Relabelling Process RelabellingFunction
  | Restriction Process (Set Label)
  deriving (Eq, Show)