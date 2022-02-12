module Test.Parser where

import Base
import Data.Either (isLeft)
import Parser
import Test.Tasty
import Test.Tasty.Hspec
import Text.Megaparsec

hspecParser :: IO TestTree
hspecParser = testSpec "Parser" $ do
  spec_parseTerm
  spec_parseVariable

spec_parseVariable :: Spec
spec_parseVariable = do
  describe "parseVariable" $ do
    it "variable 1" $
      runParser parseVariable "" "a" `shouldBe` Right "a"
    it "variable 2" $
      runParser parseVariable "" "beta" `shouldBe` Right "beta"
    it "variable 3" $
      runParser parseVariable "" "zeta123123" `shouldBe` Right "zeta123123"
    it "variable 4" $
      runParser parseVariable "" "hamma00000lambda" `shouldBe` Right "hamma00000lambda"
    it "variable 5" $
      runParser parseVariable "" "  ccc   " `shouldBe` Right "ccc"
    it "variable 6" $
      runParser parseVariable "" "qwerty   " `shouldBe` Right "qwerty"
    it "variable 7" $
      runParser parseVariable "" "    rrr45" `shouldBe` Right "rrr45"
    it "variable 8" $
      runParser parseVariable "" "  hamma00000lambda  " `shouldBe` Right "hamma00000lambda"
    it "incorrect variable 1" $
      runParser parseTerm "" "123" `shouldSatisfy` isLeft
    it "incorrect variable 2" $
      runParser parseTerm "" "0beta   " `shouldSatisfy` isLeft
    it "incorrect variable 3" $
      runParser parseTerm "" "    23zeta123123" `shouldSatisfy` isLeft
    it "incorrect variable 4" $
      runParser parseTerm "" "  9999hamma00000lambda  " `shouldSatisfy` isLeft

spec_parseTerm :: Spec
spec_parseTerm = do
  describe "parseTerm" $ do
    it "var application 1" $
      runParser parseTerm "" " x y " `shouldBe` Right (V "x" :@ V "y")
    it "var application 2" $
      runParser parseTerm "" " x x x x x " `shouldBe` Right (V "x" :@ V "x" :@ V "x" :@ V "x" :@ V "x")
    it "var application 3" $
      runParser parseTerm "" " x z (y z) " `shouldBe` Right (V "x" :@ V "z" :@ (V "y" :@ V "z"))
    it "var application 4" $
      runParser parseTerm "" " q (w (e r) t) y " `shouldBe` Right (V "q" :@ (V "w" :@ (V "e" :@ V "r") :@ V "t") :@ V "y")
    it "lambda 1" $
      runParser parseTerm "" " \\ x : a . x " `shouldBe` Right (L "x" (T "a") (V "x"))
    it "lambda 2" $
      runParser parseTerm "" " \\ xyz : alpha -> alpha . xyz (xyz xyz) " `shouldBe` Right (L "xyz" (T "alpha" :=> T "alpha") (V "xyz" :@ (V "xyz" :@ V "xyz")))
    it "lambda 3" $
      runParser parseTerm "" " \\ q : a . q w e r t y " `shouldBe` Right (L "q" (T "a") (V "q" :@ V "w" :@ V "e" :@ V "r" :@ V "t" :@ V "y"))
    it "lambda 4" $
      runParser parseTerm "" " \\ x : a . f x y " `shouldBe` Right (L "x" (T "a") (V "f" :@ V "x" :@ V "y"))
    it "combo 1" $
      runParser parseTerm "" " \\ x : a . \\ y : b . \\z : a -> b -> c . z x y " `shouldBe` Right (L "x" (T "a") (L "y" (T "b") (L "z" (T "a" :=> T "b" :=> T "c") (V "z" :@ V "x" :@ V "y"))))
    it "combo 2" $
      runParser parseTerm "" " \\ f : alpha -> alpha . \\ x : alpha . f ( f ( f x ) )" `shouldBe` Right (L "f" (T "alpha" :=> T "alpha") (L "x" (T "alpha") (V "f" :@ (V "f" :@ (V "f" :@ V "x")))))
    it "combo 3" $
      runParser parseTerm "" " (\\ x : a . x) (\\y : c -> b . y ( \\ z : c . z ))  " `shouldBe` Right (L "x" (T "a") (V "x") :@ L "y" (T "c" :=> T "b") (V "y" :@ L "z" (T "c") (V "z")))
--  describe "parseType" $ do
