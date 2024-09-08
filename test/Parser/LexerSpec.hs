module Parser.LexerSpec where

import Control.Applicative ()
import Data.Void ()
import Parser.AST (Token (..))
import Parser.Lexer (pToken)
import Test.Hspec (Spec, SpecWith, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)
import Text.Megaparsec.Char ()

baseCases :: SpecWith ()
baseCases = do
  it "should parse a simple process name" $
    parse pToken "" "P" `shouldParse` TProc "P" []
  it "should parse an action prefix" $
    parse pToken "" "a.P" `shouldParse` TPre (TActIn "a" Nothing) (TProc "P" [])

operatorCases :: SpecWith ()
operatorCases = do
  it "should parse a binary choice of simple processes" $
    parse pToken "" "P + Q" `shouldParse` TChoice (TProc "P" []) (TProc "Q" [])

spec :: Spec
spec = do
  describe "VP-CCS Parser" $ do
    describe "Base cases" baseCases
    describe "Operators" operatorCases