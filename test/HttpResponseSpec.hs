module HttpResponseSpec (spec) where

import HttpResponse
import Test.Hspec
import Utils
import Test.Hspec.QuickCheck
import qualified Data.Map as Map

-- | Test HTTP Status
testStatus = "HTTP/1.1 200 OK"
testBody = "Test body\r\n"
httpResponse = createHTTPResponse testStatus testBody
httpHeaders = headers <$> httpResponse :: IO (Map.Map String String)


expecStr :: String -> String
expecStr time = testStatus ++ "\r\n"
  ++ "Content-Length: "++ (show.length) testBody ++"\r\n"
  ++ "Content-Type: text/html\r\n"
  ++ "Date: " ++ time ++ "\r\n"
  ++ "Last-Modified: " ++ time ++ "\r\n"
  ++ "Server: Shush/0.1\r\n"
  ++ "\r\n"
  ++ testBody

spec :: Spec
spec = do
    describe "An HTTP Response" $ do
        it "has a body" $ do
            httpBody <- body <$> httpResponse
            httpBody `shouldBe` testBody
        it "has a status line" $ do
            httpStatus <- status <$> httpResponse
            httpStatus `shouldBe` testStatus
        context "In the header section" $ do
            it "has a Server Header" $ do
                server <- Map.lookup "Server" <$> httpHeaders 
                server `shouldBe` Just "Shush/0.1"
            it "has a Last-Modified Header" $ do
                lastMod <- Map.lookup "Last-Modified" <$> httpHeaders 
                time <- httpTime
                lastMod `shouldBe` Just time
            it "has a Date Header" $ do
                date <- Map.lookup "Date" <$> httpHeaders 
                time <- httpTime
                date `shouldBe` Just time
            it "has a Content Length Header" $ do
                clen <- Map.lookup "Content-Length" <$> httpHeaders 
                clen `shouldBe` Just ((show.length) testBody)
        it "is formattable into a string" $ do
            time <- httpTime
            resp <- httpResponse
            show resp `shouldBe` expecStr time
    describe "formatHeaders" $ do
        context "when given an empty map" $
            it "produces empty" $
                formatHeaders (Map.fromList []) `shouldBe` ""
        it "formats one line correctly" $
            formatHeaders (Map.fromList [("a","b")]) `shouldBe` "a: b\r\n"
        it "formats multple lines correctly" $
            formatHeaders (Map.fromList [("a","b"), ("c","d")]) `shouldBe` "a: b\r\nc: d\r\n"

main :: IO()
main = hspec spec
