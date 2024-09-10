module Parser.LexerSpec where

import AST (AExpr (..), BExpr (..), RelabellingFunction (..), RelabellingMapping (..))
import Control.Applicative ()
import Data.Function ((&))
import Data.Set (fromList)
import Data.Void ()
import Parser.AST (Token (..))
import Parser.Lexer (tokenize)
import Test.Hspec (Expectation, Spec, SpecWith, describe, it)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (ParseErrorBundle)
import Text.Megaparsec.Char ()
import Text.Megaparsec.Error (ShowErrorComponent)
import Text.Megaparsec.Stream (TraversableStream, VisualStream)

spec :: Spec
spec = do
  describe "VP-CCS Parser" $ do
    describe "Base cases" baseCases
    describe "Operators" operatorCases
    describe "Precedence" precedenceCases
    describe "Branches" branchCases
    describe "Failure cases" failureCases

baseCases :: SpecWith ()
baseCases = do
  it "should parse an empty line into nothing" $
    tokenize "" & shouldParseNone
  it "should parse a comment line into nothing" $
    tokenize "# i am a comment" & shouldParseNone
  it "should parse an assignment with leading, duplicated  and missing whitespace" $
    tokenize "   A  (x,  y,z)    =       B [  a  /b,c/d]" `shouldParseSome` TAss (TProc "A" [AVar "x", AVar "y", AVar "z"]) (TRel (TProc "B" []) (RelFn $ RelabellingFunction [RelabellingMapping "a" "b", RelabellingMapping "c" "d"]))
  it "should parse a simple process name" $
    tokenize "P" `shouldParseSome` TProc "P" []
  it "should parse the dead process" $
    tokenize "0" `shouldParseSome` TProc "0" []
  it "should parse a process name with one variable" $
    tokenize "P(x)" `shouldParseSome` TProc "P" [AVar "x"]
  it "should parse a process name with multiple variables" $
    tokenize "P(x, y, z)" `shouldParseSome` TProc "P" [AVar "x", AVar "y", AVar "z"]
  it "should parse an action prefix" $
    tokenize "a.P" `shouldParseSome` TPre (TActIn "a" Nothing) (TProc "P" [])
  it "should parse an input action with a variable" $
    tokenize "a(x).P" `shouldParseSome` TPre (TActIn "a" (Just (AVar "x"))) (TProc "P" [])
  it "should parse an output action with a variable" $
    tokenize "'a(x).P" `shouldParseSome` TPre (TActOut "a" (Just (AVar "x"))) (TProc "P" [])
  it "should parse an output action with an expression" $
    tokenize "'a(x * 2 + 1).P" `shouldParseSome` TPre (TActOut "a" (Just (Sum (Mul (AVar "x") (AVal 2)) (AVal 1)))) (TProc "P" [])
  it "should parse an internal action" $
    tokenize "τ.P" `shouldParseSome` TPre TActTau (TProc "P" [])
  it "should parse an assignment" $
    tokenize "A(x) = a(y).B(x,y)" `shouldParseSome` TAss (TProc "A" [AVar "x"]) (TPre (TActIn "a" (Just $ AVar "y")) (TProc "B" [(AVar "x"), (AVar "y")]))

operatorCases :: SpecWith ()
operatorCases = do
  it "should parse a binary choice of simple processes" $
    tokenize "P + Q" `shouldParseSome` TChoice (TProc "P" []) (TProc "Q" [])
  it "should parse a parallel composition of simple processes" $
    tokenize "P | Q" `shouldParseSome` TPar (TProc "P" []) (TProc "Q" [])
  it "should parse a prefixing with a restriction" $
    tokenize "P \\ {a}" `shouldParseSome` TRes (TProc "P" []) (ResSet $ fromList ["a"])
  it "should parse a relabelling function" $
    tokenize "P[a/b]" `shouldParseSome` TRel (TProc "P" []) (RelFn $ RelabellingFunction [RelabellingMapping "a" "b"])
  it "should parse a nested relabelling function" $
    tokenize "P[a/b, c/d]" `shouldParseSome` TRel (TProc "P" []) (RelFn $ RelabellingFunction [RelabellingMapping "a" "b", RelabellingMapping "c" "d"])

branchCases :: SpecWith ()
branchCases = do
  it "should parse a simple if-then-else branch" $
    tokenize "if x > 0 then P else Q" `shouldParseSome` TBranch (Gt (AVar "x") (AVal 0)) (TProc "P" []) (TProc "Q" [])
  it "should parse a nested if-then-else branch" $
    tokenize "if x > 0 then if y < 5 then P else Q else R" `shouldParseSome` TBranch (Gt (AVar "x") (AVal 0)) (TBranch (Lt (AVar "y") (AVal 5)) (TProc "P" []) (TProc "Q" [])) (TProc "R" [])
  it "should parse an if-then-else branch with an expression in the guard" $
    tokenize "if x - 1 * 2 > y + 9 || tt then P else Q" `shouldParseSome` TBranch (Or (Gt (Min (AVar "x") (Mul (AVal 1) (AVal 2))) (Sum (AVar "y") (AVal 9))) (BVal True)) (TProc "P" []) (TProc "Q" [])

failureCases :: SpecWith ()
failureCases = do
  it "should fail to parse an action named after a reserved keyword" $
    mapM_ (shouldFailOn tokenize) [input | kw <- ["if", "then", "else"], let input = "'" <> kw <> "(x).B"]
  it "should fail to parse a dead process with variables" $
    shouldFailOn tokenize "0(x)"
  it "should fail to parse an action with more than one expression" $
    shouldFailOn tokenize "a(x, y)"
  describe "Syntax errors" $ do
    it "should fail to parse assignments with missing braces or commas or trailing commas" $ do
      shouldFailOn tokenize "P(x = Q"
      shouldFailOn tokenize "P x = Q"
      shouldFailOn tokenize "P x) = Q"
      shouldFailOn tokenize "P(x y) = Q"
      shouldFailOn tokenize "P(x, y,) = Q"
    it "should fail to parse an operation with a missing operand" $ do
      shouldFailOn tokenize "A |"
      shouldFailOn tokenize "| B"
      shouldFailOn tokenize "A +"
      shouldFailOn tokenize "+ B"
    it "should fail to parse a relabelling with wrong channel names" $ do
      shouldFailOn tokenize "P[A/B]"
      shouldFailOn tokenize "P['b/'c]"
      shouldFailOn tokenize "P[A/b]"
      shouldFailOn tokenize "P[a/B]"

precedenceCases :: SpecWith ()
precedenceCases = do
  it "should parse restriction before relabelling" $
    tokenize "P \\ {a} [b/c]" `shouldParseSome` TRel (TRes (TProc "P" []) (ResSet $ fromList ["a"])) (RelFn $ RelabellingFunction $ [RelabellingMapping "b" "c"])
  it "should parse relabelling before restriction" $
    tokenize "P [b/c] \\ {a}" `shouldParseSome` TRes (TRel (TProc "P" []) (RelFn $ RelabellingFunction $ [RelabellingMapping "b" "c"])) (ResSet $ fromList ["a"])
  it "should parse action prefixing before composition" $
    tokenize "a.P | b.Q" `shouldParseSome` TPar (TPre (TActIn "a" Nothing) (TProc "P" [])) (TPre (TActIn "b" Nothing) (TProc "Q" []))
  it "should parse the expression with the correct precedence" $
    tokenize "a.0 | b.P \\ {d,e,f} + c.0" `shouldParseSome` TChoice (TPar (TPre (TActIn "a" Nothing) (TProc "0" [])) (TPre (TActIn "b" Nothing) (TRes (TProc "P" []) (ResSet $ fromList ["d", "e", "f"])))) (TPre (TActIn "c" Nothing) (TProc "0" []))

shouldParseSome :: (ShowErrorComponent e, VisualStream s, TraversableStream s, Show a, Eq a) => Either (ParseErrorBundle s e) (Maybe a) -> a -> Expectation
shouldParseSome bundle e = shouldParse bundle $ Just e

shouldParseNone :: (ShowErrorComponent e, VisualStream s, TraversableStream s, Show a, Eq a) => Either (ParseErrorBundle s e) (Maybe a) -> Expectation
shouldParseNone bundle = shouldParse bundle Nothing