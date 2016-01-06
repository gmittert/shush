module HttpBodySpec (spec) where

import HttpBody
import Utils
import qualified Data.ByteString.Char8 as B
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

spec :: Spec
spec = let body = createBody  (B.pack "<html><body>test</body></html>") "text/html" in
  describe "An HTTP Body" $ do
    it "has a body" $
      content body `shouldBe` B.pack "<html><body>test</body></html>"
    it "has a length" $
      contentLength body `shouldBe` (B.length.content) body 
    it "has a content type" $
      contentType body `shouldBe` "text/html"
main :: IO ()
main = hspec spec
