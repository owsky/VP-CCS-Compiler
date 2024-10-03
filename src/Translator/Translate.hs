module Translator.Translate (translateStatement) where

import Data.List (intercalate)
import Data.Set (Set, fromList, toList)
import Data.Text (Text, pack, unpack)
import Eval (evalArit, evalBool)
import Grammars.AST (AExpr (..), Label (..), RelabellingFunction (..), RelabellingMapping (..))
import Grammars.Pure_AST as Pure (Action (..), Process (..), Statement (..))
import Grammars.VP_AST as VP (Action (..), Process (..), Statement (..))
import Translator.Substitute (substitute)

-- | Translates a VP-CCS Statement into a CCS Statement
translateStatement :: Int -> VP.Statement -> Either String [Pure.Statement]
translateStatement maxInt (VP.Assignment p1 p2) = case p1 of
  (VP.ProcessName _ []) -> do
    newProc1 <- translateProcess maxInt p1
    newProc2 <- translateProcess maxInt p2
    Right [Pure.Assignment newProc1 newProc2]
  (VP.ProcessName procName ((AVar varName) : exprs)) -> do
    -- for each variable, concretize it within the nat range and substitute into p2, create a list of procnames for p1 with the concretized variables
    let m = aux 0 maxInt varName procName p2
    let stmts = [VP.Assignment (VP.ProcessName newProcName exprs) rhs | (newProcName, rhs) <- m]
    let eStatements = map (\s -> translateStatement maxInt s) stmts
    statements <- mapM id eStatements
    return $ concat statements
  _ -> error "You can only assign to process names"
  where
    aux :: Int -> Int -> Text -> Text -> VP.Process -> [(Text, VP.Process)]
    aux lowerBound higherBound _ _ _ | lowerBound > higherBound = []
    aux lowerBound higherBound varName procName rhs = do
      let newProcName = procName <> "_" <> pack (show lowerBound)
      let newRhs = substitute varName lowerBound rhs
      (newProcName, newRhs) : aux (lowerBound + 1) higherBound varName procName rhs

translateProcess :: Int -> VP.Process -> Either String Pure.Process
translateProcess _ (VP.ProcessName name exprs) = do
  let eVals = map evalArit exprs
  vals <- mapM id eVals
  let newProcName = name <> concatV vals "_"
  Right $ Pure.ProcessName newProcName []
translateProcess maxInt (VP.ActionPrefix (VP.ActionName (Input name) Nothing) proc) = do
  newProc <- (translateProcess maxInt proc)
  return $ Pure.ActionPrefix (Pure.ActionName (Input name) Nothing) newProc
translateProcess maxInt (VP.ActionPrefix (VP.ActionName (Input name) (Just expr)) proc) = case expr of
  AVar _ -> generateBoundedChoice 0 maxInt (VP.ActionPrefix (VP.ActionName (Input name) (Just expr)) proc)
  AVal val -> do
    newProc <- (translateProcess maxInt proc)
    return $ Pure.ActionPrefix (Pure.ActionName (Input $ name <> concatV [val] "_") Nothing) newProc
  _ -> error $ "Unevaluated expression while translating action prefix: " ++ show (VP.ActionPrefix (VP.ActionName (Input name) (Just expr)) proc)
translateProcess maxInt (VP.ActionPrefix (VP.ActionName (Output name) Nothing) proc) = do
  newProc <- translateProcess maxInt proc
  return $ Pure.ActionPrefix (Pure.ActionName (Output name) Nothing) newProc
translateProcess maxInt (VP.ActionPrefix (VP.ActionName (Output name) (Just var)) proc) = do
  val <- evalArit var
  let newActName = pack $ unpack name ++ "_" ++ show val
  newProc <- (translateProcess maxInt proc)
  return $ Pure.ActionPrefix (Pure.ActionName (Output newActName) Nothing) newProc
translateProcess maxInt (VP.ActionPrefix VP.Tau proc) = do
  newProc <- (translateProcess maxInt proc)
  return $ Pure.ActionPrefix (Pure.Tau) newProc
translateProcess maxInt (VP.Choice p1 p2) = do
  newProc1 <- (translateProcess maxInt p1)
  newProc2 <- (translateProcess maxInt p2)
  return $ Pure.Choice newProc1 newProc2
translateProcess maxInt (VP.Parallel p1 p2) = do
  newProc1 <- translateProcess maxInt p1
  newProc2 <- translateProcess maxInt p2
  return $ Pure.Parallel newProc1 newProc2
translateProcess maxInt (VP.Relabelling p f) = do
  newProc <- (translateProcess maxInt p)
  let relabFn = (translateRelabFn 0 maxInt f)
  return $ Pure.Relabelling newProc relabFn
translateProcess maxInt (VP.Restriction p s) = do
  newProc <- translateProcess maxInt p
  let resSet = translateRestrictSet 0 maxInt s
  return $ Pure.Restriction newProc resSet
translateProcess maxInt (VP.IfThenElse guard p1 p2) = if evalBool guard then translateProcess maxInt p1 else translateProcess maxInt p2

generateBoundedChoice :: Int -> Int -> VP.Process -> Either String Pure.Process
generateBoundedChoice lowerBound higherBound _ | lowerBound > higherBound = Right $ Pure.ProcessName "0" []
generateBoundedChoice lowerBound higherBound (VP.ActionPrefix (VP.ActionName (Input name) (Just (AVar var))) p) = do
  let newLabelName = name <> concatV [lowerBound] "_"
  let newAct = Pure.ActionName (Input newLabelName) Nothing
  newProc <- translateProcess higherBound (substitute var lowerBound p)
  recChoice <- (generateBoundedChoice (lowerBound + 1) higherBound (VP.ActionPrefix (VP.ActionName (Input name) (Just (AVar var))) p))
  return $ Pure.Choice (Pure.ActionPrefix newAct newProc) recChoice
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
    -- aux :: Int -> Int -> VP.RelabellingMapping -> [Pure.RelabellingMapping]
    aux l h _ | l > h = []
    aux l h (RelabellingMapping from to) = do
      let newFrom = pack $ unpack from ++ "_" ++ show l
      let newTo = pack $ unpack to ++ "_" ++ show l
      [RelabellingMapping newFrom newTo] <> aux (l + 1) h (RelabellingMapping from to)

translateRestrictSet :: Int -> Int -> Set Text -> Set Text
translateRestrictSet lowerBound higherBound labels = fromList . map pack . concatMap (genConcreteNames lowerBound higherBound) $ toList labels

genConcreteNames :: Int -> Int -> Text -> [String]
genConcreteNames lowerBound higherBound a = [unpack a ++ "_" ++ show i | i <- [lowerBound .. higherBound]]