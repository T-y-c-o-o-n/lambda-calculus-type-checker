module Test.Checker where

import Base
import Checker
import Data.Either (isLeft)
import Data.Map
import Test.Tasty
import Test.Tasty.Hspec

hspecChecker :: IO TestTree
hspecChecker = testSpec "Checker" $ do
  spec_check

spec_check :: Spec
spec_check = do
  describe "check" $ do
    it "simple 1" $
      check (Data.Map.empty, V "x", T "a") `shouldBe` Right (fromList [("x", T "a0")])
    it "simple 2" $
      check (Data.Map.empty, V "x", T "a -> b") `shouldBe` Right (fromList [("x", T "a0")])
    it "incorrect 1" $
      check (Data.Map.empty, L "f" (T "a0") (L "x" (T "a1") (V "f" :@ V "x")), T "a0" :=> T "a1" :=> T "a2") `shouldSatisfy` isLeft
    it "incorrect 2" $
      check (fromList [("f", T "a0"), ("x", T "a1")], V "f" :@ V "x", T "a3") `shouldSatisfy` isLeft
    it "incorrect 3" $
      check (Data.Map.empty, V "x" :@ V "x", T "a") `shouldSatisfy` isLeft
    it "incorrect 4" $
      check (fromList [("x", T "a")], V "x" :@ V "x", T "b") `shouldSatisfy` isLeft
    it "incorrect 5" $
      check (fromList [("x", T "a")], V "x" :@ V "x", T "a") `shouldSatisfy` isLeft
    it "incorrect 6" $
      check (Data.Map.empty, L "x" (T "a") (V "x" :@ V "x"), T "b") `shouldSatisfy` isLeft
    it "incorrect 7" $
      check (Data.Map.empty, L "x" (T "b") (V "x" :@ V "x"), T "a") `shouldSatisfy` isLeft
