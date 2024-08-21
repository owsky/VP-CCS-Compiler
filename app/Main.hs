module Main (main) where

import CCS.Parsers (parseProcess)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  let input = "(P + Q) | (A + B)"
  let result = parseProcess input
  print ("Input: " ++ input)
  case result of
    Left err -> putStrLn $ "Parsing error: " ++ errorBundlePretty err
    Right process -> print process