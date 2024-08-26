module CCS.Parse where

import CCS.Lexer.Lexer (tokenize)
import CCS.Parser.Grammars (Process)
import CCS.Parser.Parser (tokenToProcess)
import Data.Text (Text)
import Debug.Trace (trace)
import Text.Megaparsec.Error (errorBundlePretty)

parseInput :: Text -> Either String Process
parseInput l = do
  let toks = tokenize l
  case toks of
    Left err -> Left $ errorBundlePretty err
    Right tokens -> do
      trace ("Debug - Token: " ++ show tokens) $ do
        let proce = tokenToProcess tokens
        case proce of
          Just process -> Right process
          Nothing -> Left "Error"