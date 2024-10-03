module ProcessLines (processLines) where

import Data.Text (Text)
import Parser.Parse (parseInput)

processLines :: Int -> Text -> IO [String]
processLines maxInt inputLines = do
  let parsed = parseInput maxInt inputLines
  case parsed of
    Left err -> fail err
    Right statements -> return $ map show $ concat statements