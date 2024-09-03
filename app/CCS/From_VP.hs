module CCS.From_VP where

import CCS.Grammars (Action (..), Label (..), Process (..), RelabellingFunction (..), Statement (..))
import CCS_VP.Eval (evalBool)
import CCS_VP.Grammars (getLabelName)
import CCS_VP.Grammars qualified as VP (AExpr (..), Action (..), Label (..), Process (..), RelabellingFunction (..), Statement (..))
import Data.Set (Set)
import Data.Text (pack, unpack)

substitute :: VP.Label -> Int -> VP.Process -> VP.Process
substitute label value (VP.ProcessName name mExpr) = case mExpr of
  Just expr -> undefined
  Nothing -> VP.ProcessName name Nothing
substitute label value (VP.ActionPrefix (VP.ActionName (VP.Input name) mExpr) proc) = case mExpr of
  Just (VP.AVar varName) ->
    if varName == getLabelName label
      then
        VP.ActionPrefix (VP.ActionName (VP.Input name) (Just $ VP.AVar $ pack $ show value)) (substitute label value proc)
      else
        VP.ActionPrefix (VP.ActionName (VP.Input name) mExpr) proc
  Nothing -> VP.ActionPrefix (VP.ActionName (VP.Input name) Nothing) (substitute label value proc)
  _ -> undefined
substitute label value (VP.Choice p1 p2) = VP.Choice (substitute label value p1) (substitute label value p2)
substitute label value (VP.Parallel p1 p2) = VP.Parallel (substitute label value p1) (substitute label value p2)
substitute label value (VP.Relabelling p fn) = undefined
substitute label value (VP.Restriction p set) = undefined
substitute label value (VP.IfThenElse guard p1 p2) = undefined
substitute _ _ _ = undefined

generateBoundedChoice :: Int -> Int -> VP.Process -> Process
generateBoundedChoice lowerBound higherBound (VP.ActionPrefix (VP.ActionName (VP.Input name) (Just (VP.AVar var))) p) =
  if lowerBound <= higherBound
    then do
      let newLabelName = unpack name ++ "_" ++ show lowerBound
      let newAct = ActionName $ Input $ pack newLabelName
      let newProc = substitute (VP.Input name) lowerBound p
      Choice (ActionPrefix newAct (processFromVP newProc)) (generateBoundedChoice (lowerBound + 1) higherBound (VP.ActionPrefix (VP.ActionName (VP.Input name) (Just (VP.AVar var))) p))
    else
      ProcessName "0"
generateBoundedChoice _ _ _ = undefined

relabellingFunctionFromVP :: VP.RelabellingFunction -> RelabellingFunction
relabellingFunctionFromVP = undefined

restrictionSetFromVP :: Set VP.Label -> Set Label
restrictionSetFromVP = undefined

processFromVP :: VP.Process -> Process
processFromVP (VP.ProcessName name mExpr) = case mExpr of
  Just expr -> undefined
  Nothing -> ProcessName name
processFromVP (VP.ActionPrefix act proc) = generateBoundedChoice 0 4 (VP.ActionPrefix act proc)
processFromVP (VP.Choice p1 p2) = Choice (processFromVP p1) (processFromVP p2)
processFromVP (VP.Parallel p1 p2) = Parallel (processFromVP p1) (processFromVP p2)
processFromVP (VP.Relabelling p f) = Relabelling (processFromVP p) (relabellingFunctionFromVP f)
processFromVP (VP.Restriction p s) = Restriction (processFromVP p) (restrictionSetFromVP s)
processFromVP (VP.IfThenElse guard p1 p2) =
  if evalBool guard
    then
      processFromVP p1
    else
      processFromVP p2

statementFromVP :: VP.Statement -> Statement
statementFromVP (VP.Assignment p1 p2) = Assignment (processFromVP p1) (processFromVP p2)