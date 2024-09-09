module Eval (evalArit, evalBool) where

import AST (AExpr (..), BExpr (..))

-- | Evaluates an arithmetic expression into an integer value. Will fail if the expression contains variables
evalArit :: AExpr -> Int
evalArit (AVal val) = val
evalArit (AVar var) = error $ "Can't evaluate arithmetic expressions with variable: " ++ show var
evalArit (Sum e1 e2) = evalArit e1 + evalArit e2
evalArit (Min e1 e2) = evalArit e1 - evalArit e2
evalArit (Mul e1 e2) = evalArit e1 * evalArit e2

-- | Evaluates a Boolean expression into a Boolean value.
evalBool :: BExpr -> Bool
evalBool (BVal b) = b
evalBool (Eq e1 e2) = evalArit e1 == evalArit e2
evalBool (Lt e1 e2) = evalArit e1 < evalArit e2
evalBool (Gt e1 e2) = evalArit e1 > evalArit e2
evalBool (Not b) = not $ evalBool b
evalBool (And e1 e2) = evalBool e1 && evalBool e2
evalBool (Or e1 e2) = evalBool e1 || evalBool e2