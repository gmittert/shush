module ConfigSpec where
import SpecHelper
import Config

spec :: Spec
spec = do
    describe "ConfigSpec" $ do
        context "empty String" $ do
            it "returns empty" $ do
                getBegin "" `shouldBe` ""
main :: IO()
main = hspec spec
