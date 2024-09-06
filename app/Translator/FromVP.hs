module Translator.FromVP (statementFromVP) where

import AST (AExpr (..), Action (..), Label (..), Process (..), RelabellingFunction (..), Statement (..))
import Data.Set (Set)
import Data.Text (pack, unpack)
import Eval (evalArit, evalBool)
import Translator.Substitute (substitute)

statementFromVP :: Statement -> Statement
statementFromVP (Assignment p1 p2) = Assignment (processFromVP p1) (processFromVP p2)

processFromVP :: Process -> Process
processFromVP (ProcessName name vars) = do
  let vals = map evalArit vars
  let newProcName = pack $ unpack name ++ foldr (\val acc -> acc ++ "_" ++ show val) "" vals
  ProcessName newProcName []
processFromVP (ActionPrefix (ActionName (Input name) Nothing) proc) = ActionPrefix (ActionName (Input name) Nothing) (processFromVP proc)
processFromVP (ActionPrefix (ActionName (Input name) (Just var)) proc) = generateBoundedChoice 0 4 (ActionPrefix (ActionName (Input name) (Just var)) (processFromVP proc))
processFromVP (ActionPrefix (ActionName (Output name) Nothing) proc) = ActionPrefix (ActionName (Output name) Nothing) (processFromVP proc)
processFromVP (ActionPrefix (ActionName (Output name) (Just var)) proc) = do
  let val = evalArit var
  let newActName = pack $ unpack name ++ "_" ++ show val
  ActionPrefix (ActionName (Output newActName) Nothing) (processFromVP proc)
processFromVP (ActionPrefix Tau proc) = ActionPrefix Tau (processFromVP proc)
processFromVP (Choice p1 p2) = Choice (processFromVP p1) (processFromVP p2)
processFromVP (Parallel p1 p2) = Parallel (processFromVP p1) (processFromVP p2)
processFromVP (Relabelling p f) = Relabelling (processFromVP p) (relabellingFunctionFromVP f)
processFromVP (Restriction p s) = Restriction (processFromVP p) (restrictionSetFromVP s)
processFromVP (IfThenElse guard p1 p2) = if evalBool guard then processFromVP p1 else processFromVP p2

generateBoundedChoice :: Int -> Int -> Process -> Process
generateBoundedChoice lowerBound higherBound _ | lowerBound > higherBound = ProcessName "0" []
generateBoundedChoice lowerBound higherBound (ActionPrefix (ActionName (Input name) (Just (AVar var))) p) = do
  let newLabelName = unpack name ++ "_" ++ show lowerBound
  let newAct = ActionName (Input $ pack newLabelName) Nothing
  let newProc = substitute var lowerBound p
  Choice (ActionPrefix newAct (processFromVP newProc)) (generateBoundedChoice (lowerBound + 1) higherBound (ActionPrefix (ActionName (Input name) (Just (AVar var))) p))
generateBoundedChoice _ _ _ = undefined

relabellingFunctionFromVP :: RelabellingFunction -> RelabellingFunction
relabellingFunctionFromVP = undefined

restrictionSetFromVP :: Set Label -> Set Label
restrictionSetFromVP = undefined
