module W05CalcSpec (spec) where

import Test.Hspec
import W05Calc
import ExprT
import Parser
-- import StackVM

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "eval" $ do
    it "eval" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  describe "evalStr" $ do
    it "evalStr (2+3)*4" $ do
      evalStr "(2+3)*4" `shouldBe` Just 20
    it "evalStr 2+3*4" $ do
      evalStr "2+3*4" `shouldBe` Just 14
    it "evalStr" $ do
      evalStr "2+3*" `shouldBe` Nothing

  describe "Map" $ do
    it "withVars" $ do
      (withVars [("x", 6)] $ add (lit 3) (var "x")) `shouldBe` Just 9
    it "withVars" $ do
      (withVars [("x", 6)] $ add (lit 3) (var "y")) `shouldBe` Nothing
    it "withVars" $ do
      (withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))) `shouldBe` Just 54
