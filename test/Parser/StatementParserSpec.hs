module Parser.StatementParserSpec where

import AST
import Data.Text (Text)
import Data.Void (Void)
import Parser.StatementParser (parseLines, parseStatement)
import SpecHelper (Expectation)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (ParseErrorBundle, ShowErrorComponent)
import Text.Megaparsec.Stream (TraversableStream, VisualStream)

spec :: Spec
spec = describe "VP-CCS Statement Parser" $ do
  it "should parse an assignment with leading, duplicated  and missing whitespace" $
    parseInput "   A  (x,  y,z)    =       B [  a  /b,c/d]" `shouldParse` Assignment (ProcessName "A" [AVar "x", AVar "y", AVar "z"]) (Relabelling (ProcessName "B" []) (RelabellingFunction [RelabellingMapping "a" "b", RelabellingMapping "c" "d"]))
  it "should parse an assignment" $
    parseInput "A(x) = a(y).B(x,y)" `shouldParse` Assignment (ProcessName "A" [AVar "x"]) (ActionPrefix (ActionName (Input "a") (Just $ AVar "y")) (ProcessName "B" [(AVar "x"), (AVar "y")]))

parseInputs :: Text -> Either (ParseErrorBundle Text Void) [Statement]
parseInputs = parse parseLines ""

parseInput :: Text -> Either (ParseErrorBundle Text Void) Statement
parseInput = parse parseStatement ""

shouldParseL :: (ShowErrorComponent e, VisualStream s, TraversableStream s, Show a, Eq a) => Either (ParseErrorBundle s e) [a] -> a -> Expectation
shouldParseL bundle e = shouldParse bundle [e]