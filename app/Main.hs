module Main (main) where

import CCS.Grammars (Statement)
import CCS.Parser (parseInput)
import Control.Monad (forM)
import Data.Maybe (catMaybes)
import Data.Text (Text, lines, pack)
import Prelude hiding (lines, null)

processLine :: Text -> IO (Maybe Statement)
processLine input = do
  let parsed = parseInput input
  case parsed of
    Left err -> do
      putStrLn $ "Error: " ++ err
      return Nothing
    Right mStatement -> case mStatement of
      Just s -> return $ Just s
      Nothing -> return Nothing

processLines :: [Text] -> IO [Statement]
processLines inputLines = do
  results <- forM inputLines processLine
  return $ catMaybes results

main :: IO ()
main = do
  let inputFilePath = "programs/cs.ccs"
  let outputFilePath = inputFilePath ++ "v"
  fileContents <- readFile inputFilePath
  let inputLines = lines $ pack fileContents
  processedLines <- processLines inputLines
  let outputLines = map show processedLines
  writeFile outputFilePath (unlines outputLines)