module Translator.Translate (translateStatement, translateProcess) where

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
  (VP.ProcessName _ []) -> (\newProc1 newProc2 -> [Pure.Assignment newProc1 newProc2]) <$> translateProcess maxInt p1 <*> translateProcess maxInt p2
  (VP.ProcessName procName ((AVar varName) : exprs)) -> do
    -- for each variable, concretize it within the nat range and substitute into p2, create a list of procnames for p1 with the concretized variables
    let m = aux 0 maxInt varName procName p2
    let stmts = [VP.Assignment (VP.ProcessName newProcName exprs) rhs | (newProcName, rhs) <- m]
    statements <- mapM (\s -> translateStatement maxInt s) stmts
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
-- a process name gets its expressions evaluated (if any) and the pure process uses the concrete values in its name
translateProcess maxInt (VP.ProcessName name exprs) = do
  vals <- mapM (\e -> evalArit e maxInt) exprs
  return $ Pure.ProcessName $ name <> concatV vals "_"
-- an action prefix translation is discriminated depending on what kind of action and expression it carries
translateProcess maxInt (VP.ActionPrefix act proc) = case act of
  -- if it's a named action then the combination of label and expression determines the logic
  (VP.ActionName label mExpr) -> case (label, mExpr) of
    -- if no expression is found, create a simple pure action prefix and translate the prefixed process
    (_, Nothing) -> Pure.ActionPrefix (Pure.ActionName label) <$> translateProcess maxInt proc
    -- if it's an input action decide based upon the kind of expression
    (Input name, Just expr) -> case expr of
      -- with a variable generate all possible nd process combinations to support the full range of possible inputs
      AVar _ -> generateBoundedChoice 0 maxInt (VP.ActionPrefix (VP.ActionName (Input name) (Just expr)) proc)
      -- with a value simply create the new label and translte the prefixed process
      AVal val -> Pure.ActionPrefix (Pure.ActionName (Input $ name <> concatV [val] "_")) <$> translateProcess maxInt proc
      -- any other kind of expression is not allowed at this point
      _ -> Left $ "Unevaluated expression while translating action prefix: " ++ show (VP.ActionPrefix (VP.ActionName (Input name) (Just expr)) proc)
    -- if it's an output action, compute the new action name by evaluating the expression and translate the prefixed process
    (Output name, Just expr) -> do
      val <- evalArit expr maxInt
      let newActName = pack $ unpack name ++ "_" ++ show val
      Pure.ActionPrefix (Pure.ActionName (Output newActName)) <$> translateProcess maxInt proc
  -- internal actions remain unchanged
  (VP.Tau) -> Pure.ActionPrefix (Pure.Tau) <$> translateProcess maxInt proc
translateProcess maxInt (VP.Choice p1 p2) = Pure.Choice <$> translateProcess maxInt p1 <*> translateProcess maxInt p2
translateProcess maxInt (VP.Parallel p1 p2) = Pure.Parallel <$> translateProcess maxInt p1 <*> translateProcess maxInt p2
translateProcess maxInt (VP.Relabelling p f) = Pure.Relabelling <$> translateProcess maxInt p <*> translateRelabFn 0 maxInt f
translateProcess maxInt (VP.Restriction p s) = Pure.Restriction <$> translateProcess maxInt p <*> pure (translateRestrictSet 0 maxInt s)
translateProcess maxInt (VP.IfThenElse guard p1 p2) = if evalBool guard maxInt then translateProcess maxInt p1 else translateProcess maxInt p2

-- Given boundaries of a range and a process, generate a nd choice of pure processes based on the range of values
generateBoundedChoice :: Int -> Int -> VP.Process -> Either String Pure.Process
generateBoundedChoice lowerBound higherBound _ | lowerBound > higherBound = Right $ Pure.ProcessName "0"
generateBoundedChoice lowerBound higherBound (VP.ActionPrefix (VP.ActionName (Input name) (Just (AVar var))) p) = do
  let newLabelName = name <> concatV [lowerBound] "_"
  let newAct = Pure.ActionName (Input newLabelName)
  newProc <- translateProcess higherBound (substitute var lowerBound p)
  recChoice <- (generateBoundedChoice (lowerBound + 1) higherBound (VP.ActionPrefix (VP.ActionName (Input name) (Just (AVar var))) p))
  return $ Pure.Choice (Pure.ActionPrefix newAct newProc) recChoice
generateBoundedChoice _ _ p = error $ "Unexpected case, got: " ++ show p

-- Given a list of showables and a string, produce a text which concatenates all the showables with the given string in-between
concatV :: (Show a) => [a] -> String -> Text
concatV [] _ = ""
concatV as c = pack $ c <> intercalate c (map show as)

-- Translate a relabelling function by applying each mapping for all possible input values
translateRelabFn :: Int -> Int -> RelabellingFunction -> Either String RelabellingFunction
translateRelabFn _ _ (RelabellingFunction []) = Right $ RelabellingFunction []
translateRelabFn lowerBound higherBound (RelabellingFunction (f : fs)) = do
  -- check that the relabellings is legal, i.e., the froms must be unique
  _ <- checkLegal (f : fs)
  let headF = translateMapping lowerBound higherBound f
  RelabellingFunction mappings <- translateRelabFn lowerBound higherBound (RelabellingFunction fs)
  return $ RelabellingFunction $ headF <> mappings
  where
    translateMapping :: Int -> Int -> RelabellingMapping -> [RelabellingMapping]
    translateMapping l h _ | l > h = []
    translateMapping l h (RelabellingMapping from to) = do
      let newFrom = from <> "_" <> (pack $ show l)
      let newTo = to <> "_" <> (pack $ show l)
      [RelabellingMapping newFrom newTo] <> translateMapping (l + 1) h (RelabellingMapping from to)
    checkIn :: RelabellingMapping -> [RelabellingMapping] -> Either String ()
    checkIn _ [] = Right ()
    checkIn (RelabellingMapping to from) ((RelabellingMapping to2 from2) : xs) =
      if from == from2
        then Left $ "Found an invalid relabelling. Mapping: " ++ show (RelabellingMapping to2 from2) ++ " is remapping: " ++ show (RelabellingMapping to from)
        else checkIn (RelabellingMapping to from) xs
    checkLegal :: [RelabellingMapping] -> Either String ()
    checkLegal [] = Right ()
    checkLegal (x : xs) = do
      _ <- checkIn x xs
      checkLegal xs

-- Translate a restriction set by restricting the channels across all possible input values
translateRestrictSet :: Int -> Int -> Set Text -> Set Text
translateRestrictSet lowerBound higherBound labels = fromList . map pack . concatMap (genConcreteNames lowerBound higherBound) $ toList labels

-- Given an input range and a channel name, return a list of channel names that spans across the whole input range
genConcreteNames :: Int -> Int -> Text -> [String]
genConcreteNames lowerBound higherBound a = [unpack a ++ "_" ++ show i | i <- [lowerBound .. higherBound]]