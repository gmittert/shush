-- test-suite/ShushSpec.hs

module ShushSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Config

spec :: Spec
spec =
    describe "strip" $
        it "returns empty if given empty" $
            strip "" `shouldBe` ""
