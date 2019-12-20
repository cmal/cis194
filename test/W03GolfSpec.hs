module W03GolfSpec (spec) where

import Test.Hspec
import W03Golf

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "skips" $ do
    it "skips" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
    it "skips" $ do
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
    it "skips" $ do
      skips [1] `shouldBe` [[1]]
    it "skips" $ do
      skips [True,False] `shouldBe` [[True,False], [False]]
    it "skips" $ do
      skips ([] :: [Int]) `shouldBe` ([] :: [[Int]])

  describe "localMaxima" $ do
    it "localMaxima" $ do
      localMaxima [2,9,5,6,1] == [9,6]
    it "localMaxima" $ do
      localMaxima [2,3,4,1,5] == [4]
    it "localMaxima" $ do
      localMaxima [1,2,3,4,5] == []

  describe "histogram" $ do
    it "[3,5]" $ do
      histogram [3,5] == "   * *    \n==========\n0123456789\n"
    it "[1,1,1,5]" $ do
      histogram [1,1,1,5] == " *        \n *        \n *   *    \n==========\n0123456789\n"
    it "" $ do
      histogram [1,4,5,4,6,6,3,4,2,4,9] == "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"
