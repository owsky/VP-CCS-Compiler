module Main (main) where

import CCS.Lexer (tokenize)
import Data.Text (unpack)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  let input = "a.0 | b.P \\ {a,b} + c.0"
  putStrLn $ "Input: " ++ unpack input
  let tokens = tokenize input
  case tokens of
    Left err -> putStrLn $ errorBundlePretty err
    Right toks -> print toks