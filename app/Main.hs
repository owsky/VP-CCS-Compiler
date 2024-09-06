module Main (main) where

import Data.Maybe (mapMaybe)
import Data.Text (Text, lines, pack)
import Parser.StatementParser (parseInput)
import Translator.FromVP (statementFromVP)
import Prelude hiding (lines, null)

main :: IO ()
main = do
  let inputFilePath = "programs/cs.ccsv"
  let outputFilePath = "programs/cs.ccs"

  fileContents <- readFile inputFilePath
  let inputLines = lines $ pack fileContents

  processedLines <- processLines inputLines
  writeFile outputFilePath (unlines processedLines)

processLines :: [Text] -> IO [String]
processLines inputLines = do
  let parsed = map parseInput inputLines
  let statements = mapMaybe checkError parsed
  let output = map statementFromVP statements
  return $ map show output

checkError :: (Show a) => Either String a -> a
checkError eVal = case eVal of
  Left err -> error err
  Right val -> val