module Test.Parser where

import Base
import Data.Either (isLeft)
import Data.Map
import Parser
import Test.Tasty
import Test.Tasty.Hspec
import Text.Megaparsec

hspecParser :: IO TestTree
hspecParser = testSpec "Parser" $ do
  spec_parseVariable
  spec_parseTerm
  spec_parseType
  spec_parseContext
  spec_parseTypeInference

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

spec_parseType :: Spec
spec_parseType = do
  describe "parseType" $ do
    it "arrows 1" $
      runParser parseType "" " a -> b " `shouldBe` Right (T "a" :=> T "b")
    it "arrows 2" $
      runParser parseType "" " (a-> b ) -> c ->(     (c->d)->a) " `shouldBe` Right ((T "a" :=> T "b") :=> T "c" :=> ((T "c" :=> T "d") :=> T "a"))
    it "arrows 3" $
      runParser parseType "" " (a -> b ->( c -> (d -> e)) -> f) -> g " `shouldBe` Right ((T "a" :=> T "b" :=> (T "c" :=> (T "d" :=> T "e")) :=> T "f") :=> T "g")
    it "arrows 4" $
      runParser parseType "" " (((a)->a)->a)->a " `shouldBe` Right (((T "a" :=> T "a") :=> T "a") :=> T "a")
    it "incorrect type 1" $
      runParser parseType "" "  " `shouldSatisfy` isLeft
    it "incorrect type 2" $
      runParser parseType "" " (a -> b ->( -> (d -> e)) -> f) -> g " `shouldSatisfy` isLeft
    it "incorrect type 3" $
      runParser parseType "" " (((a)->a)->a)-> " `shouldSatisfy` isLeft
    it "incorrect type 4" $
      runParser parseType "" " (((a)->a->a)->a " `shouldSatisfy` isLeft

spec_parseContext :: Spec
spec_parseContext = do
  describe "parseContext" $ do
    it "empty 1" $
      runParser parseContext "" "" `shouldBe` Right Data.Map.empty
    it "empty 2" $
      runParser parseContext "" "  " `shouldBe` Right Data.Map.empty
    it "one type 1" $
      runParser parseContext "" "x:a" `shouldBe` Right (fromList [("x", T "a")])
    it "one type 2" $
      runParser parseContext "" "x : a->b->c" `shouldBe` Right (fromList [("x", T "a" :=> T "b" :=> T "c")])
    it "one type 3" $
      runParser parseContext "" " xyz123   :  alpha -> ( beta -> hamma ) " `shouldBe` Right (fromList [("xyz123", T "alpha" :=> (T "beta" :=> T "hamma"))])
    it "many types 1" $
      runParser parseContext "" "x:a,y:b,z:c" `shouldBe` Right (fromList [("x", T "a"), ("y", T "b"), ("z", T "c")])
    it "many types 2" $
      runParser parseContext "" " x : a->b->c   ,y:c->b->a  ,z :     asd" `shouldBe` Right (fromList [("x", T "a" :=> T "b" :=> T "c"), ("y", T "c" :=> T "b" :=> T "a"), ("z", T "asd")])
    it "many types 3" $
      runParser parseContext "" " qwerty   :  qwerty , qwerty : qwerty -> qwerty " `shouldBe` Right (fromList [("qwerty", T "qwerty"), ("qwerty", T "qwerty" :=> T "qwerty")])
    it "incorrect context 1" $
      runParser parseContext "" "x:a,y:b," `shouldSatisfy` isLeft
    it "incorrect context 2" $
      runParser parseContext "" " x :   ,y:c->b->a  ,z :     asd" `shouldSatisfy` isLeft
    it "incorrect context 3" $
      runParser parseContext "" " qwerty   :  qwerty , qwerty : " `shouldSatisfy` isLeft
    it "non-variables in context 4" $
      runParser parseContext "" " x y : a  ,y:c->b->a  ,z :     asd" `shouldSatisfy` isLeft
    it "non-variables in context 5" $
      runParser parseContext "" " x : a -> a , \\y:a.y : a -> a , z : b -> b " `shouldSatisfy` isLeft

spec_parseTypeInference :: Spec
spec_parseTypeInference = do
  describe "parseTypeInference" $ do
    it "simple 1" $
      runParser parseTypeInference "" "|-x:a" `shouldBe` Right (Data.Map.empty, V "x", T "a")
    it "simple 2" $
      runParser parseTypeInference "" "  |-  x : a   " `shouldBe` Right (Data.Map.empty, V "x", T "a")
    it "simple 3" $
      runParser parseTypeInference "" "x:a|-x:a" `shouldBe` Right (fromList [("x", T "a")], V "x", T "a")
    it "simple 4" $
      runParser parseTypeInference "" "x:a,x:a,x:a|-x:a" `shouldBe` Right (fromList [("x", T "a")], V "x", T "a")
    it "simple 5" $
      runParser parseTypeInference "" "y:b,   z:   c  |-x:a" `shouldBe` Right (fromList [("y", T "b"), ("z", T "c")], V "x", T "a")
    it "advanced 1" $
      runParser parseTypeInference "" "|-x\\y:a->a.y x:a" `shouldBe` Right (Data.Map.empty, V "x" :@ L "y" (T "a" :=> T "a") (V "y" :@ V "x"), T "a")
    it "advanced 2" $
      runParser parseTypeInference "" " xyz : abc |-  x (y z) : cba   " `shouldBe` Right (Data.Map.singleton "xyz" (T "abc"), V "x" :@ (V "y" :@ V "z"), T "cba")
    it "advanced 3" $
      runParser parseTypeInference "" "x:a,x:a,x:a,x:a|-x:a" `shouldBe` Right (fromList [("x", T "a")], V "x", T "a")
    it "advanced 4" $
      runParser parseTypeInference "" "x:a,x:a,x:a|-\\x:a.\\x:a.\\x:a.\\x:a.x:b->b->b->b->b" `shouldBe` Right (fromList [("x", T "a")], L "x" (T "a") (L "x" (T "a") (L "x" (T "a") (L "x" (T "a") (V "x")))), T "b" :=> T "b" :=> T "b" :=> T "b" :=> T "b")
    it "advanced 5" $
      runParser parseTypeInference "" "y:b->(d->e),   z:   c->c->(c->c)->c  |- x ( y z)  :   f  " `shouldBe` Right (fromList [("y", T "b" :=> T "d" :=> T "e"), ("z", T "c" :=> T "c" :=> (T "c" :=> T "c") :=> T "c")], V "x" :@ (V "y" :@ V "z"), T "f")
