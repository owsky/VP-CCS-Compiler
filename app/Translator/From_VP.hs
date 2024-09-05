module Translator.From_VP where

import AST (AExpr (..), Action (..), Label (..), Process (..), RelabellingFunction (..), Statement (..))
import Data.Set (Set)
import Data.Text (pack, unpack)
import Parser.Eval (evalBool)
import Translator.Substitute (substitute)

generateBoundedChoice :: Int -> Int -> Process -> Process
generateBoundedChoice lowerBound higherBound _
  | lowerBound > higherBound = ProcessName "0" []
generateBoundedChoice lowerBound higherBound (ActionPrefix (ActionName (Input name) [(AVar var) :: xs]) p) = do
  let newLabelName = unpack name ++ "_" ++ show lowerBound
  let newAct = ActionName (Input $ pack newLabelName) []
  let newProc = substitute name lowerBound p
  Choice (ActionPrefix newAct (processFromVP newProc)) (generateBoundedChoice (lowerBound + 1) higherBound (ActionPrefix (ActionName (Input name) [AVar var]) p))
generateBoundedChoice _ _ _ = undefined

relabellingFunctionFromVP :: RelabellingFunction -> RelabellingFunction
relabellingFunctionFromVP = undefined

restrictionSetFromVP :: Set Label -> Set Label
restrictionSetFromVP = undefined

processFromVP :: Process -> Process
processFromVP (ProcessName name mExpr) = case mExpr of
  [] -> ProcessName name []
  (_ :: xs) -> undefined
processFromVP (ActionPrefix act proc) = generateBoundedChoice 0 4 (ActionPrefix act proc)
processFromVP (Choice p1 p2) = Choice (processFromVP p1) (processFromVP p2)
processFromVP (Parallel p1 p2) = Parallel (processFromVP p1) (processFromVP p2)
processFromVP (Relabelling p f) = Relabelling (processFromVP p) (relabellingFunctionFromVP f)
processFromVP (Restriction p s) = Restriction (processFromVP p) (restrictionSetFromVP s)
processFromVP (IfThenElse guard p1 p2) =
  if evalBool guard
    then
      processFromVP p1
    else
      processFromVP p2

statementFromVP :: Statement -> Statement
statementFromVP (Assignment p1 p2) = Assignment (processFromVP p1) (processFromVP p2)