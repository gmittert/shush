-- test-suite/ShushSpec.hs

module ShushSpec (spec) where

import Shush
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

request10NoHost =  "GET somefile.html HTTP/1.0\r\n\
    \From: foo@bar.com\r\n\
    \User-Agent: someUser/1.0\r\n\
    \Host:somehost\r\n\
    \Accept:someformat\r\n\
    \\r\n\
    \Body!\r\n"


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
                getHTTPVersion request11 `shouldBe` "1.1"
        context "When given a 1.0 request" $
            it "returns 1.0" $
                getHTTPVersion request10 `shouldBe` "1.0"
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
            
main :: IO()
main = hspec spec
