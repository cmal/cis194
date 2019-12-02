module W01HanoiSpec (spec) where

import Test.Hspec
import W01Hanoi

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hanoi" $ do
    it "hanoi 1" $ do
      hanoi 1 "a" "b" "c" `shouldBe` [("a", "b")]
    it "hanoi 2" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]
