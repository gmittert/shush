module ConfigSpec where
import SpecHelper
import Config

spec :: Spec
spec =  do
    describe "configSpec.getValue" $ do 
        it "returns the corresponding value to a key" $
            getValue [("key","value")] "key" `shouldBe` "value"
        it "returns empty if the value isn't found" $
            getValue [] "key" `shouldBe` ""

    describe "ConfigSpec.dropSndHead" $ do
        context "when given the empty string for snd" $
            it "returns the empty string for snd" $
                dropSndHead ("", "") `shouldBe` ("","")
        context "when given a non empty string for snd" $
            it "drops the head of the string" $
                dropSndHead ("", "12345") `shouldBe` ("", "2345")

    describe "ConfigSpec.strip" $ do
        context "when given the empty string" $
            it "returns the empty string" $ 
                strip "" `shouldBe` ""
        context "when given no whitespace," $
            it "returns the same" $ 
                strip "abc" `shouldBe` "abc"
        context "when given only whitespace," $
            it "returns the empty string" $ 
                strip "\n\t " `shouldBe` ""

    describe "ConfigSpec.parseLine" $ do
        context "when given ':'" $
            it "returns the empty tuple" $
                parseLine ":" `shouldBe` ("","")
        context "when given ''" $
            it "returns the empty tuple" $
                parseLine "" `shouldBe` ("","")
        context "when given a:b" $
            it "returns a tuple of (a,b)" $
                parseLine "a:b" `shouldBe` ("a","b")

    describe "ConfigSpec.parseLines" $ do
        it "ignores lines starting with #" $
            parseLines ["#Comment"] `shouldBe` []
        it "Parses each given string into a tuple" $
            parseLines ["key1:value1", "key2:value2"] 
                `shouldBe` [("key1","value1"),("key2","value2")]
main :: IO()
main = hspec spec
