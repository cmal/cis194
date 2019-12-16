module W07JoinListSpec (spec) where

import Test.Hspec
import W07JoinList

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "JoinList" $ do
    it "takeJ" $ do
      jlToList (takeJ 4 t) `shouldBe` "yeah"
    it "takeJ" $ do
      jlToList (takeJ 1 t) `shouldBe` "y"
    it "takeJ" $ do
      jlToList (takeJ 0 t) `shouldBe` ""
    it "dropJ" $ do
      jlToList (dropJ 4 t) `shouldBe` ""
    it "dropJ" $ do
      jlToList (dropJ 1 t) `shouldBe` "eah"
    it "dropJ" $ do
      jlToList (dropJ 0 t) `shouldBe` "yeah"

  describe "Scrabble" $ do
    it "scoreLine" $ do
      scoreLine "yay " +++ scoreLine "haskell!" 
        `shouldBe` Append (Score 23)
                               (Single (Score 9) "yay ")
                               (Single (Score 14) "haskell!")
