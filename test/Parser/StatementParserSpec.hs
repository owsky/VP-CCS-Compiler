module Parser.StatementParserSpec where

import Data.Text (Text)
import Data.Void (Void)
import Grammars.Pure_AST (Statement (..))
import Parser.Parse (parseLines)
import Parser.StatementParser (parseStatement)
import SpecHelper (Expectation)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse, shouldSucceedOn)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (ParseErrorBundle, ShowErrorComponent)
import Text.Megaparsec.Stream (TraversableStream, VisualStream)

spec :: Spec
spec = describe "VP-CCS Statement Parser" $ do
  it "should parse an assignment with leading, duplicated  and missing whitespace" $
    shouldSucceedOn parseInput "   A  (x,  y,z)    =       B [  a  /b,c/d]"
  it "should parse an assignment" $
    shouldSucceedOn parseInput "A(x) = a(y).B(x,y)"
  it "should fail to parse assignments with missing braces or commas or trailing commas" $ do
    shouldFailOn parseInput "P(x = Q"
    shouldFailOn parseInput "P x = Q"
    shouldFailOn parseInput "P x y = Q"
    shouldFailOn parseInput "P x, y = Q"
    shouldFailOn parseInput "P x) = Q"
    shouldFailOn parseInput "P(x y) = Q"
    shouldFailOn parseInput "P(x, y,) = Q"

parseInputs :: Text -> Either (ParseErrorBundle Text Void) [[Statement]]
parseInputs = parse (parseLines 5) ""

parseInput :: Text -> Either (ParseErrorBundle Text Void) [Statement]
parseInput = parse (parseStatement 5) ""

shouldParseL :: (ShowErrorComponent e, VisualStream s, TraversableStream s, Show a, Eq a) => Either (ParseErrorBundle s e) [a] -> a -> Expectation
shouldParseL bundle e = shouldParse bundle [e]