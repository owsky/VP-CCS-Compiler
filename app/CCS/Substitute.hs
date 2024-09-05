module CCS.Substitute (substitute) where

import CCS_VP.Grammars (AExpr (..), Action (..), Process (..))
import Data.Text (Text)

concrete :: Text -> Int -> AExpr -> AExpr
concrete var val (AVar name) = if var == name then AVal val else AVar var
concrete _ _ (AVal value) = AVal value
concrete _ _ _ = undefined

-- | Given a variable name, a concrete value and a process, return a new process where each occurrence
-- | of the variable has been substituted by the value
substitute :: Text -> Int -> Process -> Process
substitute var val proc = case proc of
  (ProcessName name []) -> ProcessName name []
  (ProcessName name vars) -> ProcessName name $ map conc vars
  (ActionPrefix (ActionName label vars) p) -> ActionPrefix (ActionName label $ map conc vars) $ sub p
  (ActionPrefix Tau p) -> ActionPrefix Tau $ sub p
  (Choice p1 p2) -> Choice (sub p1) (sub p2)
  (Parallel p1 p2) -> Parallel (sub p1) (sub p2)
  (Relabelling p fn) -> Relabelling (sub p) fn
  (Restriction p set) -> Restriction (sub p) set
  (IfThenElse guard p1 p2) -> IfThenElse guard (sub p1) (sub p2)
  where
    sub = substitute var val
    conc = concrete var val