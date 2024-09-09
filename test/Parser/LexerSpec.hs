module Parser.LexerSpec where

import AST (AExpr (..), BExpr (..), RelabellingFunction (..), RelabellingMapping (..))
import Control.Applicative ()
import Data.Set (fromList)
import Data.Void ()
import Parser.AST (Token (..))
import Parser.Lexer (tokenize)
import Test.Hspec (Spec, SpecWith, describe, it)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec.Char ()

baseCases :: SpecWith ()
baseCases = do
  it "should parse an empty line into nothing" $
    tokenize "" `shouldParse` Nothing
  it "should parse a comment line into nothing" $
    tokenize "# i am a comment" `shouldParse` Nothing
  it "should parse an assignment with leading, duplicated  and missing whitespace" $
    tokenize "   A  (x,  y,z)    =       B [  a  /b,c/d]" `shouldParse` Just (TAss (TProc "A" [AVar "x", AVar "y", AVar "z"]) (TRel (TProc "B" []) (RelFn $ RelabellingFunction [RelabellingMapping "a" "b", RelabellingMapping "c" "d"])))
  it "should parse a simple process name" $
    tokenize "P" `shouldParse` Just (TProc "P" [])
  it "should parse the dead process" $
    tokenize "0" `shouldParse` Just (TProc "0" [])
  it "should parse a process name with one variable" $
    tokenize "P(x)" `shouldParse` Just (TProc "P" [AVar "x"])
  it "should parse a process name with multiple variables" $
    tokenize "P(x, y, z)" `shouldParse` Just (TProc "P" [AVar "x", AVar "y", AVar "z"])
  it "should parse an action prefix" $
    tokenize "a.P" `shouldParse` Just (TPre (TActIn "a" Nothing) (TProc "P" []))
  it "should parse an input action with a variable" $
    tokenize "a(x).P" `shouldParse` Just (TPre (TActIn "a" (Just (AVar "x"))) (TProc "P" []))
  it "should parse an output action with a variable" $
    tokenize "'a(x).P" `shouldParse` Just (TPre (TActOut "a" (Just (AVar "x"))) (TProc "P" []))
  it "should parse an output action with an expression" $
    tokenize "'a(x * 2 + 1).P" `shouldParse` Just (TPre (TActOut "a" (Just (Sum (Mul (AVar "x") (AVal 2)) (AVal 1)))) (TProc "P" []))
  it "should parse an internal action" $
    tokenize "τ.P" `shouldParse` Just (TPre TActTau (TProc "P" []))
  it "should parse an assignment" $
    tokenize "A(x) = a(y).B(x,y)" `shouldParse` Just (TAss (TProc "A" [AVar "x"]) (TPre (TActIn "a" (Just $ AVar "y")) (TProc "B" [(AVar "x"), (AVar "y")])))

operatorCases :: SpecWith ()
operatorCases = do
  it "should parse a binary choice of simple processes" $
    tokenize "P + Q" `shouldParse` Just (TChoice (TProc "P" []) (TProc "Q" []))
  it "should parse a parallel composition of simple processes" $
    tokenize "P | Q" `shouldParse` Just (TPar (TProc "P" []) (TProc "Q" []))
  it "should parse a prefixing with a restriction" $
    tokenize "P \\ {a}" `shouldParse` Just (TRes (TProc "P" []) (ResSet $ fromList ["a"]))
  it "should parse a relabelling function" $
    tokenize "P[a/b]" `shouldParse` Just (TRel (TProc "P" []) (RelFn $ RelabellingFunction [RelabellingMapping "a" "b"]))
  it "should parse a nested relabelling function" $
    tokenize "P[a/b, c/d]" `shouldParse` Just (TRel (TProc "P" []) (RelFn $ RelabellingFunction [RelabellingMapping "a" "b", RelabellingMapping "c" "d"]))

branchCases :: SpecWith ()
branchCases = do
  it "should parse a simple if-then-else branch" $
    tokenize "if x > 0 then P else Q" `shouldParse` Just (TBranch (Gt (AVar "x") (AVal 0)) (TProc "P" []) (TProc "Q" []))
  it "should parse a nested if-then-else branch" $
    tokenize "if x > 0 then if y < 5 then P else Q else R" `shouldParse` Just (TBranch (Gt (AVar "x") (AVal 0)) (TBranch (Lt (AVar "y") (AVal 5)) (TProc "P" []) (TProc "Q" [])) (TProc "R" []))
  it "should parse an if-then-else branch with an expression in the guard" $
    tokenize "if x - 1 * 2 > y + 9 || tt then P else Q" `shouldParse` Just (TBranch (Or (Gt (Min (AVar "x") (Mul (AVal 1) (AVal 2))) (Sum (AVar "y") (AVal 9))) (BVal True)) (TProc "P" []) (TProc "Q" []))

failureCases :: SpecWith ()
failureCases = do
  it "should fail to parse an action named after a reserved keyword" $
    mapM_ (shouldFailOn tokenize) [input | kw <- ["if", "then", "else"], let input = "'" <> kw <> "(x).B"]
  it "should fail to parse a dead process with variables" $
    shouldFailOn tokenize "0(x)"
  it "should fail to parse an action with more than one expression" $
    shouldFailOn tokenize "a(x, y)"
  it "should fail to parse a choice without the second process" $
    shouldFailOn tokenize "A |"
  it "should fail to parse a choice without the first process" $
    shouldFailOn tokenize "| B"

-- | Syntactically correct cases which are going to be rejected by type checking
fallThroughCases :: SpecWith ()
fallThroughCases = do
  it "should parse an assignment to an action" $
    tokenize "p = a.B" `shouldParse` Just (TAss (TActIn "p" Nothing) (TPre (TActIn "a" Nothing) (TProc "B" [])))
  it "should parse an input action with a value" $
    tokenize "a(5)" `shouldParse` Just (TActIn "a" (Just $ AVal 5))
  it "should parse an output action with a variable" $
    tokenize "'a(x)" `shouldParse` Just (TActOut "a" (Just $ AVar "x"))

precedenceCases :: SpecWith ()
precedenceCases = do
  it "should parse restriction before relabelling" $
    tokenize "P \\ {a} [b/c]" `shouldParse` Just (TRel (TRes (TProc "P" []) (ResSet $ fromList ["a"])) (RelFn $ RelabellingFunction $ [RelabellingMapping "b" "c"]))
  it "should parse relabelling before restriction" $
    tokenize "P [b/c] \\ {a}" `shouldParse` Just (TRes (TRel (TProc "P" []) (RelFn $ RelabellingFunction $ [RelabellingMapping "b" "c"])) (ResSet $ fromList ["a"]))
  it "should parse action prefixing before composition" $
    tokenize "a.P | b.Q" `shouldParse` Just (TPar (TPre (TActIn "a" Nothing) (TProc "P" [])) (TPre (TActIn "b" Nothing) (TProc "Q" [])))
  it "should parse the expression with the correct precedence" $
    tokenize "a.0 | b.P \\ {d,e,f} + c.0" `shouldParse` Just (TChoice (TPar (TPre (TActIn "a" Nothing) (TProc "0" [])) (TPre (TActIn "b" Nothing) (TRes (TProc "P" []) (ResSet $ fromList ["d", "e", "f"])))) (TPre (TActIn "c" Nothing) (TProc "0" [])))

spec :: Spec
spec = do
  describe "VP-CCS Parser" $ do
    describe "Base cases" baseCases
    describe "Operators" operatorCases
    describe "Precedence" precedenceCases
    describe "Branches" branchCases
    describe "Failure cases" failureCases
    describe "Fall through cases" fallThroughCases