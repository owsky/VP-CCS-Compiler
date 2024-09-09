module AST where

import Data.List (intercalate, sort)
import Data.Set (Set, toList)
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

-- | Extracts the label's name
getLabelName :: Label -> Text
getLabelName (Input str) = str
getLabelName (Output str) = str

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

-- | AST for relabellings, e.g., a/b
data RelabellingMapping = RelabellingMapping Text Text deriving (Eq)

instance Show RelabellingMapping where
  show :: RelabellingMapping -> String
  show (RelabellingMapping from to) = unpack from ++ "/" ++ unpack to

-- | AST for relabelling functions, e.g., [a/b, c/d]
newtype RelabellingFunction = RelabellingFunction [RelabellingMapping] deriving (Eq)

instance Show RelabellingFunction where
  show :: RelabellingFunction -> String
  show (RelabellingFunction f) = "[" ++ intercalate ", " (map show f) ++ "]"

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
  show (Sum e1 e2) = binaryShow e1 "+" e2
  show (Min e1 e2) = binaryShow e1 "-" e2
  show (Mul e1 e2) = binaryShow e1 "*" e2

-- | AST for Boolean expressions
data BExpr
  = BVal Bool
  | Eq AExpr AExpr
  | Lt AExpr AExpr
  | Gt AExpr AExpr
  | Not BExpr
  | And BExpr BExpr
  | Or BExpr BExpr
  deriving (Eq)

instance Show BExpr where
  show :: BExpr -> String
  show (BVal val) = show val
  show (Eq e1 e2) = binaryShow e1 "==" e2
  show (Lt e1 e2) = binaryShow e1 "<" e2
  show (Gt e1 e2) = binaryShow e1 ">" e2
  show (Not (BVal b)) = "¬" ++ show b
  show (Not e) = "¬(" ++ show e ++ ")"
  show (And e1 e2) = binaryShow e1 "∧" e2
  show (Or e1 e2) = binaryShow e1 "∨" e2

-- | Given two items of showable type and a string operator, concatenates them
-- into a single string
binaryShow :: (Show a) => a -> String -> a -> String
binaryShow e1 op e2 = show e1 ++ " " ++ op ++ " " ++ show e2
