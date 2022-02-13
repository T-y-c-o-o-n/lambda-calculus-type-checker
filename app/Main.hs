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
      Left (actualType, e) -> putStrLn $ "error: \"" ++ e ++ "\"; actual type is " ++ show actualType
      Right _ -> putStrLn "Correct!"
