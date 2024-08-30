module Main (main) where

import Data.Maybe (catMaybes)
import Data.Text (Text, lines, pack)
import ProcessLine (processLine)
import Prelude hiding (lines, null)

processLines :: [Text] -> IO [Text]
processLines inputLines = do
  let results = map processLine inputLines
  case sequence results of
    Left err -> do
      putStrLn $ "Error: " ++ err
      return []
    Right mStatements -> return $ catMaybes mStatements

main :: IO ()
main = do
  let inputFilePath = "programs/cs.ccsv"
  let outputFilePath = "programs/cs.ccs"

  fileContents <- readFile inputFilePath
  let inputLines = lines $ pack fileContents

  processedLines <- processLines inputLines
  let outputLines = map show processedLines
  writeFile outputFilePath (unlines outputLines)