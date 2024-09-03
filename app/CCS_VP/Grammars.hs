module CCS_VP.Grammars where

import Data.Set (Set)
import Data.Text (Text)

-- | AST for labels, i.e., channel names and their complements
data Label
  = Input Text
  | Output Text
  deriving (Eq, Ord, Show)

-- | Extracts the label's name
getLabelName :: Label -> Text
getLabelName (Input str) = str
getLabelName (Output str) = str

-- | AST for actions, i.e., transitions over channel names or internal
data Action
  = ActionName Label (Maybe AExpr)
  | Tau
  deriving (Eq, Show)

-- | AST for relabellings, e.g., a/b
data RelabellingMapping = RelabellingMapping Text Text deriving (Show, Eq)

-- | AST for relabelling functions, e.g., [a/b, c/d]
newtype RelabellingFunction = RelabellingFunction [RelabellingMapping] deriving (Show, Eq)

-- | AST for processes, i.e., process literals or process operators
data Process
  = ProcessName Text (Maybe AExpr)
  | ActionPrefix Action Process
  | Choice Process Process
  | Parallel Process Process
  | Relabelling Process RelabellingFunction
  | Restriction Process (Set Label)
  | IfThenElse BExpr Process Process
  deriving (Eq, Show)

-- | AST for statements
data Statement
  = Assignment Process Process
  deriving (Eq, Show)

-- | AST for arithmetic expressions
data AExpr
  = AVal Int
  | AVar Text
  | Sum AExpr AExpr
  | Min AExpr AExpr
  | Mul AExpr AExpr
  deriving (Eq)

instance Show AExpr where
  show :: AExpr -> String
  show (AVal val) = show val
  show (AVar var) = show var
  show (Sum e1 e2) = show e1 ++ " + " ++ show e2
  show (Min e1 e2) = show e1 ++ " - " ++ show e2
  show (Mul e1 e2) = show e1 ++ " * " ++ show e2

-- | AST for Boolean expressions
data BExpr
  = BVal Bool
  | Eq AExpr AExpr
  | Leq AExpr AExpr
  | Not BExpr
  | And BExpr BExpr
  | Or BExpr BExpr
  deriving (Eq)

instance Show BExpr where
  show :: BExpr -> String
  show (BVal val) = show val
  show (Eq e1 e2) = show e1 ++ " == " ++ show e2
  show (Leq e1 e2) = show e1 ++ " <= " ++ show e2
  show (Not (BVal b)) = "¬" ++ show b
  show (Not e) = "¬" ++ "(" ++ show e ++ ")"
  show (And e1 e2) = show e1 ++ "∧" ++ show e2
  show (Or e1 e2) = show e1 ++ "∨" ++ show e2

-- | AST for tokens, used by the parser
data Token
  = TArith AExpr
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