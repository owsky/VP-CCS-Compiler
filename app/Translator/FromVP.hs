module Translator.FromVP (statementFromVP) where

import AST (AExpr (..), Action (..), Label (..), Process (..), RelabellingFunction (..), RelabellingMapping (..), Statement (..))
import Data.List (intercalate)
import Data.Set (Set, fromList, toList)
import Data.Text (Text, pack, unpack)
import Eval (evalArit, evalBool)
import Translator.Substitute (substitute)

-- | Translates a VP-CCS Statement into a CCS Statement
statementFromVP :: Statement -> [Statement]
statementFromVP (Assignment p1 p2) = case p1 of
  (ProcessName _ []) -> [Assignment (processFromVP p1) (processFromVP p2)]
  (ProcessName procName ((AVar varName) : exprs)) -> do
    -- for each variable, concretize it within the nat range and substitute into p2, create a list of procnames for p1 with the concretized variables
    let m = aux 0 4 varName procName p2
    let stmts = [Assignment (ProcessName newProcName exprs) rhs | (newProcName, rhs) <- m]
    concatMap statementFromVP stmts
  _ -> error "You can only assign to process names"
  where
    aux :: Int -> Int -> Text -> Text -> Process -> [(Text, Process)]
    aux lowerBound higherBound _ _ _ | lowerBound > higherBound = []
    aux lowerBound higherBound varName procName rhs = do
      let newProcName = procName <> "_" <> pack (show lowerBound)
      let newRhs = substitute varName lowerBound rhs
      (newProcName, newRhs) : aux (lowerBound + 1) higherBound varName procName rhs

processFromVP :: Process -> Process
processFromVP (ProcessName name vars) = do
  let vals = map evalArit vars
  let newProcName = name <> concatV vals "_"
  ProcessName newProcName []
processFromVP (ActionPrefix (ActionName (Input name) Nothing) proc) = ActionPrefix (ActionName (Input name) Nothing) (processFromVP proc)
processFromVP (ActionPrefix (ActionName (Input name) (Just expr)) proc) = case expr of
  AVar _ -> generateBoundedChoice 0 4 (ActionPrefix (ActionName (Input name) (Just expr)) proc)
  AVal val -> ActionPrefix (ActionName (Input $ name <> concatV [val] "_") Nothing) (processFromVP proc)
  _ -> error $ "Unevaluated expression while translating action prefix: " ++ show (ActionPrefix (ActionName (Input name) (Just expr)) proc)
processFromVP (ActionPrefix (ActionName (Output name) Nothing) proc) = ActionPrefix (ActionName (Output name) Nothing) (processFromVP proc)
processFromVP (ActionPrefix (ActionName (Output name) (Just var)) proc) = do
  let val = evalArit var
  let newActName = pack $ unpack name ++ "_" ++ show val
  ActionPrefix (ActionName (Output newActName) Nothing) (processFromVP proc)
processFromVP (ActionPrefix Tau proc) = ActionPrefix Tau (processFromVP proc)
processFromVP (Choice p1 p2) = Choice (processFromVP p1) (processFromVP p2)
processFromVP (Parallel p1 p2) = Parallel (processFromVP p1) (processFromVP p2)
processFromVP (Relabelling p f) = Relabelling (processFromVP p) (relabellingFunctionFromVP 0 4 f)
processFromVP (Restriction p s) = Restriction (processFromVP p) (restrictionSetFromVP 0 4 s)
processFromVP (IfThenElse guard p1 p2) = if evalBool guard then processFromVP p1 else processFromVP p2

generateBoundedChoice :: Int -> Int -> Process -> Process
generateBoundedChoice lowerBound higherBound _ | lowerBound > higherBound = ProcessName "0" []
generateBoundedChoice lowerBound higherBound (ActionPrefix (ActionName (Input name) (Just (AVar var))) p) = do
  let newLabelName = name <> concatV [lowerBound] "_"
  let newAct = ActionName (Input newLabelName) Nothing
  let newProc = substitute var lowerBound p
  Choice (ActionPrefix newAct (processFromVP newProc)) (generateBoundedChoice (lowerBound + 1) higherBound (ActionPrefix (ActionName (Input name) (Just (AVar var))) p))
generateBoundedChoice _ _ p = error $ "Unexpected case, got: " ++ show p

concatV :: (Show a) => [a] -> String -> Text
concatV [] _ = ""
concatV as c = pack $ c <> intercalate c (map show as)

relabellingFunctionFromVP :: Int -> Int -> RelabellingFunction -> RelabellingFunction
relabellingFunctionFromVP _ _ (RelabellingFunction []) = RelabellingFunction []
relabellingFunctionFromVP lowerBound higherBound (RelabellingFunction (f : fs)) =
  RelabellingFunction $ aux lowerBound higherBound f <> mappings
  where
    RelabellingFunction mappings = relabellingFunctionFromVP lowerBound higherBound (RelabellingFunction fs)
    aux :: Int -> Int -> RelabellingMapping -> [RelabellingMapping]
    aux l h _ | l > h = []
    aux l h (RelabellingMapping from to) = do
      let newFrom = pack $ unpack from ++ "_" ++ show l
      let newTo = pack $ unpack to ++ "_" ++ show l
      [RelabellingMapping newFrom newTo] <> aux (l + 1) h (RelabellingMapping from to)

restrictionSetFromVP :: Int -> Int -> Set Text -> Set Text
restrictionSetFromVP lowerBound higherBound labels = fromList . map pack . concatMap (generateConcreteNames lowerBound higherBound) $ toList labels

generateConcreteNames :: Int -> Int -> Text -> [String]
generateConcreteNames lowerBound higherBound a = [unpack a ++ "_" ++ show i | i <- [lowerBound .. higherBound]]