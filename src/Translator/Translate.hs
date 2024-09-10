module Translator.Translate (translateStatement) where

import AST (AExpr (..), Action (..), Label (..), Process (..), RelabellingFunction (..), RelabellingMapping (..), Statement (..))
import Data.List (intercalate)
import Data.Set (Set, fromList, toList)
import Data.Text (Text, pack, unpack)
import Eval (evalArit, evalBool)
import Translator.Substitute (substitute)

-- | Translates a VP-CCS Statement into a CCS Statement
translateStatement :: Int -> Statement -> [Statement]
translateStatement maxInt (Assignment p1 p2) = case p1 of
  (ProcessName _ []) -> [Assignment (translateProcess maxInt p1) (translateProcess maxInt p2)]
  (ProcessName procName ((AVar varName) : exprs)) -> do
    -- for each variable, concretize it within the nat range and substitute into p2, create a list of procnames for p1 with the concretized variables
    let m = aux 0 maxInt varName procName p2
    let stmts = [Assignment (ProcessName newProcName exprs) rhs | (newProcName, rhs) <- m]
    concatMap (\s -> translateStatement maxInt s) stmts
  _ -> error "You can only assign to process names"
  where
    aux :: Int -> Int -> Text -> Text -> Process -> [(Text, Process)]
    aux lowerBound higherBound _ _ _ | lowerBound > higherBound = []
    aux lowerBound higherBound varName procName rhs = do
      let newProcName = procName <> "_" <> pack (show lowerBound)
      let newRhs = substitute varName lowerBound rhs
      (newProcName, newRhs) : aux (lowerBound + 1) higherBound varName procName rhs

translateProcess :: Int -> Process -> Process
translateProcess _ (ProcessName name vars) = do
  let vals = map evalArit vars
  let newProcName = name <> concatV vals "_"
  ProcessName newProcName []
translateProcess maxInt (ActionPrefix (ActionName (Input name) Nothing) proc) = ActionPrefix (ActionName (Input name) Nothing) (translateProcess maxInt proc)
translateProcess maxInt (ActionPrefix (ActionName (Input name) (Just expr)) proc) = case expr of
  AVar _ -> generateBoundedChoice 0 maxInt (ActionPrefix (ActionName (Input name) (Just expr)) proc)
  AVal val -> ActionPrefix (ActionName (Input $ name <> concatV [val] "_") Nothing) (translateProcess maxInt proc)
  _ -> error $ "Unevaluated expression while translating action prefix: " ++ show (ActionPrefix (ActionName (Input name) (Just expr)) proc)
translateProcess maxInt (ActionPrefix (ActionName (Output name) Nothing) proc) = ActionPrefix (ActionName (Output name) Nothing) (translateProcess maxInt proc)
translateProcess maxInt (ActionPrefix (ActionName (Output name) (Just var)) proc) = do
  let val = evalArit var
  let newActName = pack $ unpack name ++ "_" ++ show val
  ActionPrefix (ActionName (Output newActName) Nothing) (translateProcess maxInt proc)
translateProcess maxInt (ActionPrefix Tau proc) = ActionPrefix Tau (translateProcess maxInt proc)
translateProcess maxInt (Choice p1 p2) = Choice (translateProcess maxInt p1) (translateProcess maxInt p2)
translateProcess maxInt (Parallel p1 p2) = Parallel (translateProcess maxInt p1) (translateProcess maxInt p2)
translateProcess maxInt (Relabelling p f) = Relabelling (translateProcess maxInt p) (translateRelabFn 0 maxInt f)
translateProcess maxInt (Restriction p s) = Restriction (translateProcess maxInt p) (translateRestrictSet 0 maxInt s)
translateProcess maxInt (IfThenElse guard p1 p2) = if evalBool guard then translateProcess maxInt p1 else translateProcess maxInt p2

generateBoundedChoice :: Int -> Int -> Process -> Process
generateBoundedChoice lowerBound higherBound _ | lowerBound > higherBound = ProcessName "0" []
generateBoundedChoice lowerBound higherBound (ActionPrefix (ActionName (Input name) (Just (AVar var))) p) = do
  let newLabelName = name <> concatV [lowerBound] "_"
  let newAct = ActionName (Input newLabelName) Nothing
  let newProc = substitute var lowerBound p
  Choice (ActionPrefix newAct (translateProcess higherBound newProc)) (generateBoundedChoice (lowerBound + 1) higherBound (ActionPrefix (ActionName (Input name) (Just (AVar var))) p))
generateBoundedChoice _ _ p = error $ "Unexpected case, got: " ++ show p

concatV :: (Show a) => [a] -> String -> Text
concatV [] _ = ""
concatV as c = pack $ c <> intercalate c (map show as)

translateRelabFn :: Int -> Int -> RelabellingFunction -> RelabellingFunction
translateRelabFn _ _ (RelabellingFunction []) = RelabellingFunction []
translateRelabFn lowerBound higherBound (RelabellingFunction (f : fs)) =
  RelabellingFunction $ aux lowerBound higherBound f <> mappings
  where
    RelabellingFunction mappings = translateRelabFn lowerBound higherBound (RelabellingFunction fs)
    aux :: Int -> Int -> RelabellingMapping -> [RelabellingMapping]
    aux l h _ | l > h = []
    aux l h (RelabellingMapping from to) = do
      let newFrom = pack $ unpack from ++ "_" ++ show l
      let newTo = pack $ unpack to ++ "_" ++ show l
      [RelabellingMapping newFrom newTo] <> aux (l + 1) h (RelabellingMapping from to)

translateRestrictSet :: Int -> Int -> Set Text -> Set Text
translateRestrictSet lowerBound higherBound labels = fromList . map pack . concatMap (genConcreteNames lowerBound higherBound) $ toList labels

genConcreteNames :: Int -> Int -> Text -> [String]
genConcreteNames lowerBound higherBound a = [unpack a ++ "_" ++ show i | i <- [lowerBound .. higherBound]]