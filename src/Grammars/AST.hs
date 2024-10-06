module Grammars.AST where

import Data.List (intercalate)
import Data.Text (Text, unpack)

-- | AST for labels, i.e., channel names and their complements
data Label
  = Input Text
  | Output Text
  deriving (Eq, Ord)

instance Show Label where
  show :: Label -> String
  show (Input name) = unpack name
  show (Output name) = "\'" <> unpack name

-- | Extracts the label's name
getLabelName :: Label -> Text
getLabelName (Input str) = str
getLabelName (Output str) = str

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

-- | AST for relabellings, e.g., a/b
data RelabellingMapping = RelabellingMapping Text Text deriving (Eq)

instance Show RelabellingMapping where
  show :: RelabellingMapping -> String
  show (RelabellingMapping to from) = unpack to ++ "/" ++ unpack from

-- | AST for relabelling functions, e.g., [a/b, c/d]
data RelabellingFunction = RelabellingFunction [RelabellingMapping] deriving (Eq)

instance Show RelabellingFunction where
  show :: RelabellingFunction -> String
  show (RelabellingFunction f) = "[" ++ intercalate ", " (map show f) ++ "]"