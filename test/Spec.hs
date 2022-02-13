import Test.Checker (hspecChecker)
import Test.Parser (hspecParser)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: IO TestTree
tests = do
  parser <- hspecParser
  checker <- hspecChecker
  return $ testGroup "All Tests" [parser, checker]

main :: IO ()
main = tests >>= defaultMain

