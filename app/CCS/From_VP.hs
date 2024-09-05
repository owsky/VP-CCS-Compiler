module CCS.From_VP where

import CCS.Grammars (Action (..), Label (..), Process (..), RelabellingFunction (..), Statement (..))
import CCS.Substitute (substitute)
import CCS_VP.Eval (evalBool)
import CCS_VP.Grammars qualified as VP (AExpr (..), Action (..), Label (..), Process (..), RelabellingFunction (..), Statement (..))
import Data.Set (Set)
import Data.Text (pack, unpack)

generateBoundedChoice :: Int -> Int -> VP.Process -> Process
generateBoundedChoice lowerBound higherBound (VP.ActionPrefix (VP.ActionName (VP.Input name) [(VP.AVar var) :: xs]) p) =
  if lowerBound <= higherBound
    then do
      let newLabelName = unpack name ++ "_" ++ show lowerBound
      let newAct = ActionName $ Input $ pack newLabelName
      let newProc = substitute name lowerBound p
      Choice (ActionPrefix newAct (processFromVP newProc)) (generateBoundedChoice (lowerBound + 1) higherBound (VP.ActionPrefix (VP.ActionName (VP.Input name) [VP.AVar var]) p))
    else
      ProcessName "0"
generateBoundedChoice _ _ _ = undefined

relabellingFunctionFromVP :: VP.RelabellingFunction -> RelabellingFunction
relabellingFunctionFromVP = undefined

restrictionSetFromVP :: Set VP.Label -> Set Label
restrictionSetFromVP = undefined

processFromVP :: VP.Process -> Process
processFromVP (VP.ProcessName name mExpr) = case mExpr of
  [] -> ProcessName name
  (_ :: xs) -> undefined
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