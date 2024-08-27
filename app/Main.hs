module Main (main) where

import CCS.Parser (parseInput)
import Data.Text (unpack)

main :: IO ()
main = do
  let input = "A + B = A"
  -- let input = "a.0 | b.P \\ {a,b} + c.0"
  -- let input = "CS = 'pub.'coin.coffee.CS"
  -- let input = "A = τ.Z | a.B + 'a.C"
  -- let input = "CS = (a.A + B) | τ.Z"
  putStrLn $ "Input: " ++ unpack input
  let parsed = parseInput input
  case parsed of
    Left err -> putStrLn err
    Right p -> print p