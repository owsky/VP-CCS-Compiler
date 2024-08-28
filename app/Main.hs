module Main (main) where

import CCS.Grammars (Statement)
import CCS.Parser (parseInput)
import Control.Monad (forM)
import Data.Maybe (catMaybes)
import Data.Text (Text, lines, null, pack, strip)
import Prelude hiding (lines, null)

processLine :: Text -> IO (Maybe Statement)
processLine input
  | null (strip input) = return Nothing
  | otherwise = do
      let parsed = parseInput input
      case parsed of
        Left err -> do
          putStrLn $ "Error: " ++ err
          return Nothing
        Right p -> return (Just p)

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