module ProcessLines (processLines) where

import AST (Statement)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Parser.StatementParser (parseInput)
import Translator.Translate (translateStatement)

processLines :: Int -> [Text] -> IO [String]
processLines maxInt inputLines = do
  let parsed = map parseInput inputLines
  let statements = mapMaybe checkError parsed
  let output = concatMap (\s -> translateStatement maxInt s) statements
  return $ map show output
  where
    checkError :: Either String (Maybe Statement) -> Maybe Statement
    checkError eVal = case eVal of
      Left err -> error err
      Right val -> val