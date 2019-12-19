module W10AParserSpec (spec) where

import Test.Hspec
import W10AParser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "abParser" $ do
    it "abParser fail" $ do
      runParser abParser "ae" `shouldBe` Nothing
    it "abParser success" $ do
      runParser abParser "abc" `shouldBe` Just (('a','b'),"c")

  describe "abParser_" $ do
    it "abParser_ fail" $ do
      runParser abParser_ "ae" `shouldBe` Nothing
    it "abParser_ success" $ do
      runParser abParser_ "abc" `shouldBe` Just ((), "c")

  describe "intOrUppercase" $ do
    it "342abcd" $ do
      runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
    it "XYZ" $ do
      runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")
    it "foo" $ do
      runParser intOrUppercase "foo" `shouldBe` Nothing

