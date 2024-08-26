module Main (main) where

import CCS.Parse (parseInput)
import Data.Text (unpack)

main :: IO ()
main = do
  -- let input = "a.0 | b.P \\ {a,b} + c.0"
  -- let input = "CS = 'pub.'coin.coffee.CS"
  -- let input = "A = τ.Z | a.B + 'a.C"
  let input = "A[a/b]"
  putStrLn $ "Input: " ++ unpack input
  let parsed = parseInput input
  case parsed of
    Left err -> putStrLn err
    Right p -> print p