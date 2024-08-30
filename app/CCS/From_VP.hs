module CCS.From_VP where

import CCS.Grammars (Action (..), Label, Process (..), RelabellingFunction, Statement (..))
import CCS_VP.Grammars qualified as VP (Action (..), Label, Process (..), RelabellingFunction, Statement (..))
import Data.Set (Set)

actionFromVP :: VP.Action -> Action
actionFromVP = undefined

relabellingFunctionFromVP :: VP.RelabellingFunction -> RelabellingFunction
relabellingFunctionFromVP = undefined

restrictionSetFromVP :: Set VP.Label -> Set Label
restrictionSetFromVP = undefined

processFromVP :: VP.Process -> Process
processFromVP (VP.ProcessName name mExpr) = case mExpr of
  Just expr -> undefined
  Nothing -> ProcessName name
processFromVP (VP.ActionPrefix act proc) = ActionPrefix (actionFromVP act) (processFromVP proc)
processFromVP (VP.Choice p1 p2) = Choice (processFromVP p1) (processFromVP p2)
processFromVP (VP.Parallel p1 p2) = Parallel (processFromVP p1) (processFromVP p2)
processFromVP (VP.Relabelling p f) = Relabelling (processFromVP p) (relabellingFunctionFromVP f)
processFromVP (VP.Restriction p s) = Restriction (processFromVP p) (restrictionSetFromVP s)
processFromVP (VP.IfThenElse guard p1 p2) = undefined

statementFromVP :: VP.Statement -> Statement
statementFromVP (VP.Assignment p1 p2) = Assignment (processFromVP p1) (processFromVP p2)