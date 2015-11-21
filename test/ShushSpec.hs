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
            it "should return only headers" $ do
                header <- genHeader 0
                response <- createResponse ""
                response `shouldBe` status200_10 ++ header
        context "When given a body" $
            it "should contruct headers and prepend to the body" $ do
                    header <- genHeader 4
                    response <- createResponse "test"
                    response `shouldBe` status200_10 ++ header ++ "\r\n" ++ "test"
main :: IO()
main = hspec spec
