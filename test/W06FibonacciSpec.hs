module W06FibonacciSpec (spec) where

import Test.Hspec
import W06Fibonacci

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fib1" $ do
    it "fib1" $ do
