module HttpRequestSpec (spec) where

import HttpRequest
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Map as Map

spec :: Spec
request11 =  "GET somefile.html HTTP/1.1\r\n\
    \From: foo@bar.com\r\n\
    \User-Agent: someUser/1.0\r\n\
    \Host:somehost\r\n\
    \\r\n\
    \Body!\r\n"
parsedRequest11 = HTTPRequest
    HTTP_11
    GET
    (Map.fromList [("From","foo@bar.com"),
        ("User-Agent","someUser/1.0"),
        ("Host","somehost")])
    "Body!\r\n"


request11Bad =  "GET somefile.html HTTP/1.1\r\n\
    \From: foo@bar.com\r\n\
    \User-Agent: someUser/1.0\r\n\
    \Accept:someformat\r\n\
    \\r\n\
    \Body!\r\n"

request10 =  "GET somefile.html HTTP/1.0\r\n\
    \From: foo@bar.com\r\n\
    \User-Agent: someUser/1.0\r\n\
    \Accept:someformat\r\n\
    \\r\n\
    \Body!\r\n"
parsedRequest10 = HTTPRequest
    HTTP_10
    GET
    (Map.fromList [("From","foo@bar.com"),
        ("User-Agent","someUser/1.0"),
        ("Accept","someformat")])
    "Body!\r\n"

request10NoHost =  "GET somefile.html HTTP/1.0\r\n\
    \From: foo@bar.com\r\n\
    \User-Agent: someUser/1.0\r\n\
    \Host:somehost\r\n\
    \Accept:someformat\r\n\
    \\r\n\
    \Body!\r\n"

request11NoBody =  "GET somefile.html HTTP/1.1\r\n\
    \From: foo@bar.com\r\n\
    \User-Agent: someUser/1.0\r\n\
    \Host:somehost\r\n"
parsedRequest11NoBody = HTTPRequest
    HTTP_11
    GET
    (Map.fromList [("From","foo@bar.com"),
        ("User-Agent","someUser/1.0"),
        ("Host","somehost")])
    []

spec = do
    describe "Shush.validate" $ do
        context "When given an invalid HTTP1.1 request" $
            it "returns false" $
                validate request11Bad `shouldBe` False
        context "When given a valid HTTP1.1 request" $
            it "returns true" $
                validate request11 `shouldBe` True
        context "When given a valid HTTP1.0 request" $
            it "returns true" $ do
                validate request10 `shouldBe` True
                validate request10NoHost `shouldBe` True

    describe "Shush.getHTTPVersion" $ do
        context "When given a 1.1 request" $
            it "returns 1.1" $
                getHTTPVersion request11 `shouldBe` HTTP_11
        context "When given a 1.0 request" $
            it "returns 1.0" $
                getHTTPVersion request10 `shouldBe` HTTP_10

    describe "Shush.parseHeaders" $
        context "When given an HTTP request" $
            it "returns a Map of its headers" $
                parseHeaders request11 `shouldBe` Map.fromList [("From","foo@bar.com"),("User-Agent","someUser/1.0"),("Host","somehost")]

    describe "Shush.hasHost" $ do
        context "If the request has a Host field" $
            it "returns true" $
                hasHost request11 `shouldBe` True
        context "If the request doesn't have a Host field" $
            it "returns false " $
                hasHost request11Bad `shouldBe` False
            
    describe "Shush.parseRequest" $ do
        context "If given a valid 1.0 request" $ 
            it "Should successfully generate a struct" $
                parseRequest request10 `shouldBe` Right parsedRequest10
        context "If given a valid 1.1 request" $
            it "Should successfully generate a struct" $
                parseRequest request11 `shouldBe` Right parsedRequest11
        context "If given a valid 1.1 request with no body" $
            it "Should successfully generate a struct" $
                parseRequest request11NoBody `shouldBe` Right parsedRequest11NoBody
        context "If given an invalid 1.1 request" $
            it "Should return False" $
                parseRequest request11Bad `shouldBe` Left False

    describe "Shush.getStatusLine" $
        it "returns the status line of a request" $
            getStatusLine request11 `shouldBe` "GET somefile.html HTTP/1.1\r"

    describe "Shush.getBody" $ do
        context "If the request has a body" $
            it "returns the body of a request" $
                getBody request11 `shouldBe` "Body!\r\n"
        context "If the request does not have a body" $
            it "returns []" $
                getBody request11NoBody `shouldBe` ""
    describe "Shush.isNewline" $ do
        it "returns true for \n" $
                isNewline ((head.lines) "\n") `shouldBe` True
        it "returns true for \r\n" $
                isNewline ((head.lines) "\r\n") `shouldBe` True
        it "returns false for just others" $
                isNewline "sad" `shouldBe` False
main :: IO()
main = hspec spec
