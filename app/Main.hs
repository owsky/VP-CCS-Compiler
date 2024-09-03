module Main (main) where

-- import Data.Maybe (catMaybes)
-- import Data.Text (Text, lines, pack)

-- import Prelude hiding (lines, null)

-- processLines :: [Text] -> IO [Text]
-- processLines inputLines = do
--   let results = map processLine inputLines
--   case sequence results of
--     Left err -> do
--       putStrLn $ "Error: " ++ err
--       return []
--     Right mStatements -> return $ catMaybes mStatements

-- main :: IO ()
-- main = do
--   let inputFilePath = "programs/cs.ccsv"
--   let outputFilePath = "programs/cs.ccs"

--   fileContents <- readFile inputFilePath
--   let inputLines = lines $ pack fileContents

--   processedLines <- processLines inputLines
--   let outputLines = map show processedLines
--   writeFile outputFilePath (unlines outputLines)

import CCS.From_VP (statementFromVP)
import CCS_VP.Grammars (Statement (..))
import CCS_VP.StatementParser (parseInput)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

processLine :: Text -> Statement
processLine line = do
  let parsed = parseInput line
  case parsed of
    Left _ -> undefined
    Right mStatement -> fromMaybe undefined mStatement

main :: IO ()
main = do
  let input :: Text = pack "P = a(x).b(x).P"
  let s = processLine input
  print $ statementFromVP s