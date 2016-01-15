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
                , getExtension
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
            return $ createBody body fileName
          Left _ -> return body404
      HEAD -> case file of
          Right handle -> do
            body <- B.readFile fileName
            hClose handle
            return $ createBody (B.pack []) fileName
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

-- | Returns the extension of a Filename where the extension is
-- | the characters after the final '.'
getExtension :: Filename -> String
getExtension "" = ""
getExtension (x:xs)
  | takeWhile (/= '.') (x:xs) == (x:xs) = ""
  | otherwise = reverse $ helper "" (x:xs)
  where
  helper :: String -> String -> String
  helper acc (x:xs)
    | null xs = if x == '.' then "" else x:acc
    | x == '.' = helper "" xs
    | otherwise = helper (x:acc) xs

-- | A pdf files starts with the bytes: %PDF (0x25 0x50 0x44 0x46)
isPDF :: B.ByteString -> Bool
isPDF bs = B.take 4 bs == B.pack [0x25, 0x50, 0x44, 0x46]

-- | Guesses the type of the body media
-- | Handles only pdf and html files
guessMediaType :: B.ByteString -> String -> String
guessMediaType bs f
  | extension == "pdf" && isPDF bs = "application/pdf"
  | extension == "html" = "text/html"
  | otherwise = "application/octet-stream"
  where extension = getExtension f

-- | Creates an HTTPBody from a ByteString and content type. Exported
-- | for testing purposes only, use bodyFromRequest instead.
createBody :: B.ByteString -> Filename -> HTTPBody
createBody bs f = HTTPBody bs (B.length bs) (guessMediaType bs f) HTTP200

-- | Creates an HTTPBody from a string and content type
createBodyStr :: String -> Filename -> HTTPBody
createBodyStr s =  createBody (BC.pack s)
