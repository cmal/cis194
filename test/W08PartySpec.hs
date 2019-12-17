module W08PartySpec (spec) where

import Test.Hspec
import Employee
import W08Party

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "GuestList" $ do
    it "maxFun" $ do
       (getFun $ maxFun testCompany) `shouldBe` 26
