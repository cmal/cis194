module W06FibonacciSpec (spec) where

import Test.Hspec
import W06Fibonacci

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fib" $ do
    it "fib" $ do
       fib 0 `shouldBe` 1
    it "fib" $ do
       fib 10 `shouldBe` 89

  describe "fibs1" $ do
    it "fibs1" $ do
       take 10 fibs1 `shouldBe` [1,1,2,3,5,8,13,21,34,55]

  describe "fibs2" $ do
    it "fibs2" $ do
      take 10 fibs2 `shouldBe` [1,1,2,3,5,8,13,21,34,55]


  describe "fib3" $ do
    it "fib3" $ do
      show fib3 `shouldBe` "0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,..."
    it "fib3_" $ do
      take 10 fib3_ `shouldBe` [1,1,2,3,5,8,13,21,34,55]

  describe "fib4" $ do
    it "fib4 0" $ do
      fib4 0 `shouldBe` 1
    it "fib4 10" $ do
      fib4 10 `shouldBe` 89
