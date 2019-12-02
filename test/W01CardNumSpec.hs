module CardNumSpec (spec) where

import Test.Hspec
import W01CardNum

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toDigits/toDigitsRev" $ do
    it "Convert number to a list of digits" $ do
      toDigits 1234 `shouldBe` [1,2,3,4]
    it "Convert number to a list of digits reverted" $ do
      toDigitsRev 1234 `shouldBe` [4,3,2,1]
    it "Convert 0 to an empty list" $ do
      toDigits 0 `shouldBe` []
    it "Convert negative number to an empty list" $ do
      toDigits (-17) `shouldBe` []
  

  describe "doubleEveryOther" $ do
    it "Doubles the correct digit" $ do
      doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]

    it "Doubles the correct digit" $ do
      doubleEveryOther [1,2,3] `shouldBe` [1,4,3]
       

  describe "sumDigits" $ do
    it "Add the digits of the doubled values and the undoubled digits from the original number" $ do
      sumDigits [16,7,12,5] `shouldBe` 22


  describe "validate" $ do
    it "Whether 4012888888881881 is a valid credit card number" $ do
      validate 4012888888881881 `shouldBe` True

    it "Whether 4012888888881882 is a valid credit card number" $ do
      validate 4012888888881882 `shouldBe` False
  
