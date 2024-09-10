module ProcessLines (processLines) where

import Data.Text (Text)
import Parser.StatementParser (parseInput)
import Translator.Translate (translateStatement)

processLines :: Int -> Text -> IO [String]
processLines maxInt inputLines = do
  let parsed = parseInput inputLines
  case parsed of
    Left err -> error err
    Right statements -> do
      let output = concatMap (\s -> translateStatement maxInt s) statements
      return $ map show output