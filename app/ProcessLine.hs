module ProcessLine where

import Data.Text (Text, pack)
import Parser.StatementParser (parseInput)
import Translator.From_VP (statementFromVP)

processLine :: Text -> Either String (Maybe Text)
processLine line = do
  let parsed = parseInput line
  case parsed of
    Left err -> Left ("Error: " ++ err)
    Right mStatement -> case mStatement of
      Nothing -> Right Nothing
      Just statement -> Right $ Just $ pack (show (statementFromVP statement))