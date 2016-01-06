{-|
Module        : HttpBody
Description  :  HTTP body functions for Shush
Copyright     : (c) Jason Mittertreiner, 2015
License       : GPL-3
Maintainer    : jmittert@uwaterloo.ca
Stability     : experimental
Portability   : Unix

This module contains functions for creating and
manipulating HttpBodies
 -}
module HttpBody (HTTPBody
                , StatusCode
                , contentType
                , content
                , contentLength
                , status
                , bodyFromRequest
                , body404
                , createBodyStr
                , createBody) where
import Utils
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import HttpRequest
import Config
import System.IO
import System.IO.Error
import Control.Exception

data HTTPBody =
    HTTPBody { -- | The body of the HttpBody
               content  :: B.ByteString,
               -- | The length of the body
               contentLength :: Int,
               -- | The content type contained by the body
               contentType :: String,
               -- | Indicates success of creating the body
               status :: StatusCode
               } deriving (Eq, Show)

-- | Given an HTTP Request and a Config, produces an appropriate 
-- | body, handling IO and possible 404s
bodyFromRequest :: HTTPRequest -> Config -> IO HTTPBody
bodyFromRequest req config = do
    let http_path = getValue config "http_path"
    let fileName = http_path ++ "/" ++ uri req
    file <- tryIOError (openFile fileName ReadMode)::IO (Either IOError Handle)
    case method req of
      GET -> case file of
          Right handle -> do
            body <- B.readFile fileName
            hClose handle
            return $ createBody body "application/octet-stream"
          Left _ -> return body404
      HEAD -> case file of
          Right handle -> do
            body <- B.readFile fileName
            hClose handle
            return $ createBody (B.pack []) "application/octetstream"
          Left _ -> return body404
      _ -> return body404

-- | Body for a HTTP 404 response
-- | Creates a body indicating a 404
body404 =  HTTPBody http404Body (B.length http404Body) "text/html" HTTP404
  where http404Body = BC.pack "<html><head><title>404 Not Found</title></head><body><p><strong>404 Not Found</strong></p></body></html>\r\n"

-- | Body for a HTTP 405 response
-- | Creates a body indicating a 405
body405 =  HTTPBody http404Body (B.length http404Body) "text/html" HTTP405
  where http404Body = BC.pack "<html><head><title>405 Method Not Allowed</title></head><body><p><strong>405 Method Not Allowed</strong></p></body></html>\r\n"

-- | Creates an empty body indicating a 404
body404Empty =  HTTPBody http404Body (B.length http404Body) "text/html" HTTP404
  where http404Body = B.pack []

-- | Creates an HTTPBody from a ByteString and content type. Exported
-- | for testing purposes only, use bodyFromRequest instead.
createBody :: B.ByteString -> String -> HTTPBody
createBody bStr cType = HTTPBody bStr (B.length bStr) cType HTTP200

-- | Creates an HTTPBody from a string and content type
createBodyStr :: String -> String -> HTTPBody
createBodyStr str cType =  let bStr = BC.pack str in
  createBody bStr cType
