module CCS.Grammars where

import Data.Set (Set)
import Data.Text (Text)

-- | AST for labels, i.e., channel names and their complements
data Label
  = Input Text
  | Output Text
  deriving (Eq, Ord, Show)

getLabelName :: Label -> Text
getLabelName (Input str) = str
getLabelName (Output str) = str

-- | AST for actions, i.e., transitions over channel names or internal
data Action
  = ActionName Label
  | Tau
  deriving (Eq, Show)

data RelabellingMapping = RelabellingMapping Text Text deriving (Show, Eq)

newtype RelabellingFunction = RelabellingFunction [RelabellingMapping] deriving (Show, Eq)

-- | AST for processes, i.e., process literals or process operators
data Process
  = ProcessName Text
  | ActionPrefix Action Process
  | Sum Process Process
  | Parallel Process Process
  | Relabelling Process RelabellingFunction
  | Restriction Process (Set Label)
  deriving (Eq, Show)

-- | AST for statements
data Statement
  = Assignment Text Process
  deriving (Show)

-- | AST for tokens, used by the parser
data Token
  = TokId Text
  | RelFn RelabellingFunction
  | ResSet (Set Label)
  | TPre Token Token
  | TSum Token Token
  | TPar Token Token
  | TRes Token Token
  | TRel Token Token
  | TAss Token Token
  deriving (Eq, Show)