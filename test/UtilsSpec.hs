module UtilsSpec where
import Test.Hspec
import Utils

spec :: Spec
spec =  do
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
