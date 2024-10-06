module Eval (evalArit, evalBool) where

import Data.Text (unpack)
import Grammars.AST (AExpr (..), BExpr (..))

checkRange :: Int -> Int -> Int
checkRange val maxInt
  | val < 0 = 0
  | val > maxInt = maxInt
  | otherwise = val

-- | Evaluates an arithmetic expression into an integer value. Will fail if the expression contains variables
evalArit :: AExpr -> Int -> Either String Int
evalArit (AVal val) maxInt = Right $ checkRange val maxInt
evalArit (AVar var) _ = Left $ "Found unbound variable: " ++ unpack var
evalArit (Sum e1 e2) maxInt = checkRange <$> ((+) <$> evalArit e1 maxInt <*> evalArit e2 maxInt) <*> pure maxInt
evalArit (Min e1 e2) maxInt = checkRange <$> ((-) <$> evalArit e1 maxInt <*> evalArit e2 maxInt) <*> pure maxInt
evalArit (Mul e1 e2) maxInt = checkRange <$> ((*) <$> evalArit e1 maxInt <*> evalArit e2 maxInt) <*> pure maxInt

-- | Evaluates a Boolean expression into a Boolean value.
evalBool :: BExpr -> Int -> Bool
evalBool (BVal b) _ = b
evalBool (Eq e1 e2) maxInt = evalArit e1 maxInt == evalArit e2 maxInt
evalBool (Lt e1 e2) maxInt = evalArit e1 maxInt < evalArit e2 maxInt
evalBool (Gt e1 e2) maxInt = evalArit e1 maxInt > evalArit e2 maxInt
evalBool (Not b) maxInt = not $ evalBool b maxInt
evalBool (And e1 e2) maxInt = evalBool e1 maxInt && evalBool e2 maxInt
evalBool (Or e1 e2) maxInt = evalBool e1 maxInt || evalBool e2 maxInt