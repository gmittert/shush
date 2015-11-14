module UtilsSpec where
import SpecHelper
import Utils

spec :: Spec
spec =  do
    describe "Utils.dropSndHead" $ do
        context "when given the empty string for snd" $
            it "returns the empty string for snd" $
                dropSndHead ("", "") `shouldBe` ("","")
        context "when given a non empty string for snd" $
            it "drops the head of the string" $
                dropSndHead ("", "12345") `shouldBe` ("", "2345")

    describe "Utils.strip" $ do
        context "when given the empty string" $
            it "returns the empty string" $ 
                strip "" `shouldBe` ""
        context "when given no whitespace," $
            it "returns the same" $ 
                strip "abc" `shouldBe` "abc"
        context "when given only whitespace," $
            it "returns the empty string" $ 
                strip "\n\t " `shouldBe` ""

    describe "Utils.parseLine" $ do
        context "when given ':'" $
            it "returns the empty tuple" $
                parseLine ":" `shouldBe` ("","")
        context "when given ''" $
            it "returns the empty tuple" $
                parseLine "" `shouldBe` ("","")
        context "when given a:b" $
            it "returns a tuple of (a,b)" $
                parseLine "a:b" `shouldBe` ("a","b")
main :: IO()
main = hspec spec
