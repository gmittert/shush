module HttpBodySpec (spec) where

import HttpBody
import Utils
import qualified Data.ByteString.Char8 as B
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

spec :: Spec
spec = let body = createBody  (B.pack "<html><body>test</body></html>") "text/html" in do
  describe "An HTTP Body" $ do
    it "has a body" $
      content body `shouldBe` B.pack "<html><body>test</body></html>"
    it "has a length" $
      contentLength body `shouldBe` (B.length.content) body 
    it "has a content type" $
      contentType body `shouldBe` "application/octet-stream"
  describe "getExtension" $ do
    it "returns empty for empty" $
      getExtension "" `shouldBe` ""
    it "returns empty files with no extension" $
      getExtension "noextension" `shouldBe` ""
    it "returns the extension if it has one" $
      getExtension "noextension.ext" `shouldBe` "ext"
    it "handles edge cases" $ do
      getExtension ".ext" `shouldBe` "ext"
      getExtension "ext." `shouldBe` ""
main :: IO ()
main = hspec spec
