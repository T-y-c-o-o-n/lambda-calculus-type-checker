import Test.Parser (hspecParser)
import Test.Checker (hspecChecker)
import Test.Tasty (defaultMain, TestTree, testGroup)

tests :: IO TestTree
tests = do
  parser <- hspecParser
  checker <- hspecChecker
  return $ testGroup "All Tests" [parser, checker]

main :: IO ()
main = tests >>= defaultMain
