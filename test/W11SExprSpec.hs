module W11SExprSpec (spec) where

import Test.Hspec
import W11AParser
import W11SExpr
import Data.Char

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "zeroOrMore" $ do
    it "isUpper" $ do
      runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC","dEfgH")
    it "isUpper 2" $ do
      runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Just ("","abcdeFGh")

  describe "oneOrMore" $ do
    it "isUpper" $ do
      runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Just ("ABC","dEfgH")
    it "isUpper 2" $ do
      runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Nothing

  describe "spaces" $ do
    it "   111" $ do
      runParser spaces "   111" `shouldBe` Just ("   ", "111")

  describe "ident" $ do
    it "foobar baz" $ do
      runParser ident "foobar baz" `shouldBe` Just ("foobar"," baz")
    it "foo33fA" $ do
      runParser ident "foo33fA" `shouldBe` Just ("foo33fA", "")
    it "2bad" $ do
      runParser ident "2bad" `shouldBe` Nothing
    it "empty" $ do
      runParser ident "" `shouldBe` Nothing

  describe "sexpr" $ do
    it "5" $ do
      runParser parseSExpr "5" `shouldNotBe` Nothing
    it "foo3" $ do
      runParser parseSExpr "foo3" `shouldNotBe` Nothing
    it "(bar (foo) 3 5 874)" $ do
      runParser parseSExpr "(bar (foo) 3 5 874)" `shouldNotBe` Nothing
    it "(((lambda x (lambda y (plus x y))) 3) 5)" $ do
      runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)" `shouldNotBe` Nothing
    it "( lots of ( spaces in ) this ( one ) )" $ do
      runParser parseSExpr "( lots of ( spaces in ) this ( one ) )" `shouldNotBe` Nothing
