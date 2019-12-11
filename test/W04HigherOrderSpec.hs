module W04HigherOrderSpec (spec) where

import Test.Hspec
import W04HigherOrder

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "xor" $ do
    it "xor [False, True, False]" $ do
      xor [False, True, False] `shouldBe` True
    it "xor [False, True, False, False, True]" $ do
      xor [False, True, False, False, True] `shouldBe` False

  describe "map'" $ do
    it "map'" $ do
      map' id [False, True, False] `shouldBe` [False, True, False]

  describe "myFoldl" $ do
    it "+" $ do
      myFoldl (+) 0 [1..4] `shouldBe` 10
    it "-" $ do
      myFoldl (-) 0 [1..4] `shouldBe` foldl (-) 0 [1..4]
    it "-" $ do
      myFoldl (-) 0 [1..4] `shouldNotBe` foldr (-) 0 [1..4]

  describe "sieve Sundaram" $ do
    it "2" $ do
      sieveSundaram 2 `shouldBe` [3,5]
    it "21" $ do
      sieveSundaram 21 `shouldBe` [3,5,7,11,13,17,19,23,29,31,37,41,43]

  describe "fold Tree" $ do
    it "foldTree empty" $ do
      foldTree "" `shouldBe` Leaf
    it "foldTree A" $ do
      foldTree "A" `shouldBe` Node 0 Leaf 'A' Leaf
    it "foldTree ABCDEFGHIJ" $ do
      foldTree "ABCDEFGHIJ" `shouldBe` Node 3 (Node 2 (Node 1 (Node 0 Leaf 'D' Leaf) 'G' Leaf) 'I' (Node 1 (Node 0 Leaf 'A' Leaf) 'E' Leaf)) 'J' (Node 2 (Node 1 (Node 0 Leaf 'B' Leaf) 'F' Leaf) 'H' (Node 0 Leaf 'C' Leaf))
