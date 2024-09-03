module CCS_VP.Eval where

import CCS_VP.Grammars (AExpr (..), BExpr (..))

evalArit :: AExpr -> Int
evalArit (AVal val) = val
evalArit (AVar _) = undefined
evalArit (Sum e1 e2) = evalArit e1 + evalArit e2
evalArit (Min e1 e2) = evalArit e1 - evalArit e2
evalArit (Mul e1 e2) = evalArit e1 * evalArit e2

evalBool :: BExpr -> Bool
evalBool (BVal b) = b
evalBool (Eq e1 e2) = evalArit e1 == evalArit e2
evalBool (Leq e1 e2) = evalArit e1 <= evalArit e2
evalBool (Not b) = not $ evalBool b
evalBool (And e1 e2) = evalBool e1 && evalBool e2
evalBool (Or e1 e2) = evalBool e1 || evalBool e2