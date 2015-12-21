module ConfigSpec where
import Config
import Test.Hspec

spec :: Spec
spec =  do
    describe "configSpec.getValue" $ do
        it "returns the corresponding value to a key" $
            getValue [("key","value")] "key" `shouldBe` "value"
        it "returns empty if the value isn't found" $
            getValue [] "key" `shouldBe` ""

    describe "ConfigSpec.parseLines" $ do
        it "ignores lines starting with #" $
            parseLines ["#Comment"] `shouldBe` []
        it "Parses each given string into a tuple" $
            parseLines ["key1:value1", "key2:value2"]
                `shouldBe` [("key1","value1"),("key2","value2")]
        it "Parses can handle empty lines" $
            parseLines ["", "key2:value2"]
                `shouldBe` [("key2","value2")]
main :: IO()
main = hspec spec
