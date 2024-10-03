module Translator.Substitute (substitute) where

import Data.Text (Text)
import Grammars.AST (AExpr (..), BExpr (..))
import Grammars.VP_AST (Action (..), Process (..))

-- | Concretizes all the variables in the given arithmetic expression which match
-- the provided variable name by replacing the variable with the given value
concreteA :: Text -> Int -> AExpr -> AExpr
concreteA var val (AVar name) = if var == name then AVal val else AVar name
concreteA _ _ (AVal value) = AVal value
concreteA var val (Sum e1 e2) = Sum (concreteA var val e1) (concreteA var val e2)
concreteA var val (Min e1 e2) = Min (concreteA var val e1) (concreteA var val e2)
concreteA var val (Mul e1 e2) = Mul (concreteA var val e1) (concreteA var val e2)

-- | Concretizes all the variables in the given boolean expression which match
-- the provided variable name by replacing the variable with the given value
concreteB :: Text -> Int -> BExpr -> BExpr
concreteB _ _ (BVal b) = BVal b
concreteB var val (Eq e1 e2) = Eq (concreteA var val e1) (concreteA var val e2)
concreteB var val (Lt e1 e2) = Lt (concreteA var val e1) (concreteA var val e2)
concreteB var val (Gt e1 e2) = Gt (concreteA var val e1) (concreteA var val e2)
concreteB var val (Not e) = Not $ concreteB var val e
concreteB var val (And e1 e2) = And (concreteB var val e1) (concreteB var val e2)
concreteB var val (Or e1 e2) = Or (concreteB var val e1) (concreteB var val e2)

-- | Given a variable name, a concrete value and a process, return a new process where each occurrence
-- | of the variable has been substituted by the value
substitute :: Text -> Int -> Process -> Process
substitute var val proc = case proc of
  (ProcessName name []) -> ProcessName name []
  (ProcessName name vars) -> ProcessName name $ map concA vars
  (ActionPrefix (ActionName label Nothing) p) -> ActionPrefix (ActionName label Nothing) (sub p)
  (ActionPrefix (ActionName label (Just expr)) p) -> ActionPrefix (ActionName label (Just $ concA expr)) (sub p)
  (ActionPrefix Tau p) -> ActionPrefix Tau $ sub p
  (Choice p1 p2) -> Choice (sub p1) (sub p2)
  (Parallel p1 p2) -> Parallel (sub p1) (sub p2)
  (Relabelling p fn) -> Relabelling (sub p) fn
  (Restriction p set) -> Restriction (sub p) set
  (IfThenElse guard p1 p2) -> IfThenElse (concB guard) (sub p1) (sub p2)
  where
    sub = substitute var val
    concA = concreteA var val
    concB = concreteB var val