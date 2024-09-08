module Parser.LexerSpec where

import Control.Applicative ()
import Data.Void ()
import Parser.AST (Token (TActIn, TPre, TProc))
import Parser.Lexer (pToken)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)
import Text.Megaparsec.Char ()

spec :: Spec
spec =
  describe "Parser" $
    do
      it "should parse a simple process name" $
        parse pToken "" "P" `shouldParse` TProc "P"
      it "should parse an action prefix" $
        parse pToken "" "a.P" `shouldParse` TPre (TActIn "a") (TProc "P")