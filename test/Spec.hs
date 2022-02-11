import Test.Parser (hspecParser)
import Test.Tasty (defaultMain, TestTree, testGroup)

tests :: IO TestTree
tests = do
  parser <- hspecParser
  return $ testGroup "All Tests" [parser]

main :: IO ()
main = tests >>= defaultMain
