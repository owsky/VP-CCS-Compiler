module ProcessLine where

import CCS.From_VP (statementFromVP)
import CCS_VP.StatementParser (parseInput)
import Data.Text (Text, pack)

processLine :: Text -> Either String (Maybe Text)
processLine line = do
  let parsed = parseInput line
  case parsed of
    Left err -> Left ("Error: " ++ err)
    Right mStatement -> case mStatement of
      Nothing -> Right Nothing
      Just statement -> Right $ Just $ pack (show (statementFromVP statement))