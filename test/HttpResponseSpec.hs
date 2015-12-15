module HttpResponseSpec (spec) where

import HttpResponse
import Utils
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Map as Map

-- | Test HTTP Status
testStatus = "HTTP/1.1 200 OK\r\n"
testBody = "Test body"
httpResponse = createHTTPResponse testStatus testBody
httpHeaders = headers <$> httpResponse :: IO (Map.Map String String)

spec :: Spec
spec =
    describe "An HTTP Response" $ do
        it "has a body" $ do
            httpBody <- body <$> httpResponse
            httpBody `shouldBe` testBody
        it "has a status line" $ do
            httpStatus <- status <$> httpResponse
            httpStatus `shouldBe` testStatus
        context "In the header section" $ 
            it "has a Server Header" $ do
                server <- Map.lookup "Server" <$> httpHeaders 
                server `shouldBe` Just "Shush/0.1"
main :: IO()
main = hspec spec
