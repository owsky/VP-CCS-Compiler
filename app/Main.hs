module Main (main) where

import AST (Statement)
import Data.Maybe (mapMaybe)
import Data.Text (Text, lines, pack)
import Parser.StatementParser (parseInput)
import Translator.Translate (translateStatement)
import Prelude hiding (lines, null)

main :: IO ()
main = do
  let inputFilePath = "programs/cs.vccs"
  let outputFilePath = "programs/cs.ccs"

  fileContents <- readFile inputFilePath
  let inputLines = lines $ pack fileContents

  processedLines <- processLines inputLines
  writeFile outputFilePath (unlines processedLines)

processLines :: [Text] -> IO [String]
processLines inputLines = do
  let parsed = map parseInput inputLines
  let statements = mapMaybe checkError parsed
  let output = concatMap translateStatement statements
  return $ map show output
  where
    checkError :: Either String (Maybe Statement) -> Maybe Statement
    checkError eVal = case eVal of
      Left err -> error err
      Right val -> val