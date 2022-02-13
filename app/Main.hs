module Main where

import Checker
import Parser
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  input <- getLine
  case parse input of
    Left e -> putStrLn $ errorBundlePretty e
    Right ti -> case check ti of
      Left e -> putStrLn e
      Right _ -> putStrLn "Correct!"
