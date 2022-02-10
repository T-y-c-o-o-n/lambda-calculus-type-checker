module Main where

import Parser
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  input <- getLine
  case parse input of
    Left e -> putStrLn $ errorBundlePretty e
    Right t -> putStrLn "Correct"
