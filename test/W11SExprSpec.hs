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
