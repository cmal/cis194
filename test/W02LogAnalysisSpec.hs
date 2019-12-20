module W02LogAnalysisSpec (spec) where

import Test.Hspec
import W02Log
import W02LogAnalysis

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "parseMessage" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"

  describe "parse" $ do
    it "testParse error.log" $ do
      testParse parse 2 "src/error.log" `shouldReturn`
        [ LogMessage Info 5053 "pci_id: con ing!"
        , LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)"
        ]
    it "testParse sample.log" $ do
      testParse parse 2 "src/sample.log" `shouldReturn`
        [ LogMessage Info 6 "Completed armadillo processing"
        , LogMessage Info 1 "Nothing to report"
        ]
      
  describe "whatWentWrong" $ do
    it "testWhatWentWrong error.log" $ do
      testWhatWentWrong parse whatWentWrong "src/error.log" `shouldReturn` []
    it "testWhatWentWrong sample" $ do
      testWhatWentWrong parse whatWentWrong "src/sample.log" `shouldReturn` 
        [ "Way too many pickles"
        , "Bad pickle-flange interaction detected"
        , "Flange failed!"
        ]
      

