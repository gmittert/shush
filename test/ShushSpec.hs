-- test-suite/ShushSpec.hs

module ShushSpec (spec) where

import Shush
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Map as Map

spec :: Spec
spec =
    describe "Shush.createResponse" $ do
        context "When given an empty body" $
            it "should return only headers" $
                createResponse "" `shouldBe` status200_10 ++ genHeader 0
        context "When given a body" $
            it "should contruct headers and prepend to the body" $
                createResponse "test" `shouldBe` 
                    status200_10 ++ (genHeader 4 ++ "\r\n" ++ "test")
main :: IO()
main = hspec spec
