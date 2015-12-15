-- test-suite/ShushSpec.hs

module ShushSpec (spec) where

import Shush
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Map as Map

spec :: Spec
spec = 
    describe "Shush.createResponse" $
        context "When given an empty body" $
            it "should return only headers" $
                1 `shouldBe` 2-1
main :: IO()
main = hspec spec
