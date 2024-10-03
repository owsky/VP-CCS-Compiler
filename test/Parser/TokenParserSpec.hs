module Parser.TokenParserSpec where

import Control.Applicative ()
import Data.Set (fromList)
import Data.Text (Text)
import Data.Void (Void)
import Grammars.AST (AExpr (..), BExpr (..), RelabellingFunction (..), RelabellingMapping (..))
import Parser.Token (Token (..))
import Parser.TokenParser (pToken)
import Test.Hspec (Spec, SpecWith, describe, it)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (MonadParsec (eof), ParseErrorBundle, parse)

spec :: Spec
spec = describe "VP-CCS Token Parser" $ do
  describe "Base cases" baseCases
  describe "Operators" operatorCases
  describe "Precedence" precedenceCases
  describe "Branches" branchCases
  describe "Failure cases" failureCases

baseCases :: SpecWith ()
baseCases = do
  it "should parse a simple process name" $
    parseToken "P" `shouldParse` TProc "P" []
  it "should parse the dead process" $
    parseToken "0" `shouldParse` TProc "0" []
  it "should parse a process name with one variable" $
    parseToken "P(x)" `shouldParse` TProc "P" [AVar "x"]
  it "should parse a process name with multiple variables" $
    parseToken "P(x, y, z)" `shouldParse` TProc "P" [AVar "x", AVar "y", AVar "z"]
  it "should parse an action prefix" $
    parseToken "a.P" `shouldParse` TPre (TActIn "a" Nothing) (TProc "P" [])
  it "should parse an input action with a variable" $
    parseToken "a(x).P" `shouldParse` TPre (TActIn "a" (Just (AVar "x"))) (TProc "P" [])
  it "should parse an output action with a variable" $
    parseToken "'a(x).P" `shouldParse` TPre (TActOut "a" (Just (AVar "x"))) (TProc "P" [])
  it "should parse an output action with an expression" $
    parseToken "'a(x * 2 + 1).P" `shouldParse` TPre (TActOut "a" (Just (Sum (Mul (AVar "x") (AVal 2)) (AVal 1)))) (TProc "P" [])
  it "should parse an internal action" $
    parseToken "τ.P" `shouldParse` TPre TActTau (TProc "P" [])

operatorCases :: SpecWith ()
operatorCases = do
  it "should parse a binary choice of simple processes" $
    parseToken "P + Q" `shouldParse` TChoice (TProc "P" []) (TProc "Q" [])
  it "should parse a parallel composition of simple processes" $
    parseToken "P | Q" `shouldParse` TPar (TProc "P" []) (TProc "Q" [])
  it "should parse a prefixing with a restriction" $
    parseToken "P \\ {a}" `shouldParse` TRes (TProc "P" []) (ResSet $ fromList ["a"])
  it "should parse a relabelling function" $
    parseToken "P[a/b]" `shouldParse` TRel (TProc "P" []) (RelFn $ RelabellingFunction [RelabellingMapping "a" "b"])
  it "should parse a nested relabelling function" $
    parseToken "P[a/b, c/d]" `shouldParse` TRel (TProc "P" []) (RelFn $ RelabellingFunction [RelabellingMapping "a" "b", RelabellingMapping "c" "d"])

branchCases :: SpecWith ()
branchCases = do
  it "should parse a simple if-then-else branch" $
    parseToken "if x > 0 then P else Q" `shouldParse` TBranch (Gt (AVar "x") (AVal 0)) (TProc "P" []) (TProc "Q" [])
  it "should parse a nested if-then-else branch" $
    parseToken "if x > 0 then if y < 5 then P else Q else R" `shouldParse` TBranch (Gt (AVar "x") (AVal 0)) (TBranch (Lt (AVar "y") (AVal 5)) (TProc "P" []) (TProc "Q" [])) (TProc "R" [])
  it "should parse an if-then-else branch with an expression in the guard" $
    parseToken "if x - 1 * 2 > y + 9 || tt then P else Q" `shouldParse` TBranch (Or (Gt (Min (AVar "x") (Mul (AVal 1) (AVal 2))) (Sum (AVar "y") (AVal 9))) (BVal True)) (TProc "P" []) (TProc "Q" [])

failureCases :: SpecWith ()
failureCases = do
  it "should fail to parse an action named after a reserved keyword" $
    mapM_ (shouldFailOn parseToken) [input | kw <- ["if", "then", "else"], let input = "'" <> kw <> "(x).B"]
  it "should fail to parse a dead process with variables" $
    shouldFailOn parseToken "0(x)"
  it "should fail to parse an action with more than one expression" $
    shouldFailOn parseToken "a(x, y)"
  describe "Syntax errors" $ do
    it "should fail to parse an operation with a missing operand" $ do
      shouldFailOn parseToken "A |"
      shouldFailOn parseToken "| B"
      shouldFailOn parseToken "A +"
      shouldFailOn parseToken "+ B"
    it "should fail to parse a relabelling with wrong channel names" $ do
      shouldFailOn parseToken "P[A/B]"
      shouldFailOn parseToken "P['b/'c]"
      shouldFailOn parseToken "P[A/b]"
      shouldFailOn parseToken "P[a/B]"

precedenceCases :: SpecWith ()
precedenceCases = do
  it "should parse restriction before relabelling" $
    parseToken "P \\ {a} [b/c]" `shouldParse` TRel (TRes (TProc "P" []) (ResSet $ fromList ["a"])) (RelFn $ RelabellingFunction $ [RelabellingMapping "b" "c"])
  it "should parse relabelling before restriction" $
    parseToken "P [b/c] \\ {a}" `shouldParse` TRes (TRel (TProc "P" []) (RelFn $ RelabellingFunction $ [RelabellingMapping "b" "c"])) (ResSet $ fromList ["a"])
  it "should parse action prefixing before composition" $
    parseToken "a.P | b.Q" `shouldParse` TPar (TPre (TActIn "a" Nothing) (TProc "P" [])) (TPre (TActIn "b" Nothing) (TProc "Q" []))
  it "should parse the expression with the correct precedence" $
    parseToken "a.0 | b.P \\ {d,e,f} + c.0" `shouldParse` TChoice (TPar (TPre (TActIn "a" Nothing) (TProc "0" [])) (TPre (TActIn "b" Nothing) (TRes (TProc "P" []) (ResSet $ fromList ["d", "e", "f"])))) (TPre (TActIn "c" Nothing) (TProc "0" []))

parseToken :: Text -> Either (ParseErrorBundle Text Void) Token
parseToken = parse (pToken <* eof) ""