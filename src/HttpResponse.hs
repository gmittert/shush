{-|
Module        : HttpResponse
Description  :  HTTP functions for Shush
Copyright     : (c) Jason Mittertreiner, 2015
License       : GPL-3
Maintainer    : jmittert@uwaterloo.ca
Stability     : experimental
Portability   : Unix

This module contains various HTTPResponse related functions
 -}
module HttpResponse (HTTPResponse,
                     createHTTPResponse,
                     createHTTP404,
                     HttpResponse.status,
                     body,
                     headers,
                     headerStr,
                     formatHeaders,
                     formatMetadata
        ) where
import qualified Data.Map.Strict as Map
import Utils
import qualified HttpBody as HB

data HTTPResponse =
    HTTPResponse { -- | The method of the request, (GET, POST, PUT, etc)
                   status :: String,
                   -- | String representation requested uri
                   headers :: Map.Map String String,
                   -- | String representation of the request body
                   body    :: HB.HTTPBody
                } deriving (Eq)

formatMetadata :: HTTPResponse -> String
formatMetadata resp = status resp ++ "\r\n"
  ++ formatHeaders (headers resp)
  ++ "\r\n"

-- | Creates an HTTPResponse out of a status line and body
createHTTPResponse :: HB.HTTPBody -> IO HTTPResponse
createHTTPResponse body = do
      time <- httpTime
      let headers = genHeader body time in
        return $ case HB.status body of
          HTTP200 -> HTTPResponse status200_10 headers body
          HTTP405 -> HTTPResponse status405_10 headers body
          HTTP404 -> HTTPResponse status404_10 headers body

-- | Utility Method to create a 404 response
createHTTP404 :: IO HTTPResponse
createHTTP404 = createHTTPResponse HB.body404

-- | Generates appriate headers for a body length and time
genHeader :: HB.HTTPBody -> String -> Map.Map String String
genHeader httpBody time =
    Map.fromList [("Server", "Shush/0.1"),
        ("Last-Modified", time),
        ("Content-Type", HB.contentType httpBody),
        ("Date", time),
        ("Content-Length", (show.HB.contentLength) httpBody)]

-- | Formats a Map String String as
-- | Key: Value\r\n
-- | Key: Value\r\n
-- | ...
-- | Key: Value\r\n
formatHeaders :: Map.Map String String -> String
formatHeaders = Map.foldrWithKey' (\key val y -> key ++ ": " ++ val ++ "\r\n" ++ y) ""

-- | Returns the headers of a response in a String formatted as
-- | Key: Value\r\n
-- | Key: Value\r\n
-- | ...
-- | Key: Value\r\n
headerStr :: HTTPResponse -> String
headerStr req = formatHeaders (headers req)

-- | HTTP 200 1.0 response
status200_10 = "HTTP/1.0 200 OK"
-- | HTTP 200 1.1 response
status200_11 = "HTTP/1.1 200 OK"
-- | HTTP 404 1.1 response
status404_11 = "HTTP/1.1 404 Not Found"
-- | HTTP 404 1.0 response
status404_10 = "HTTP/1.0 404 Not Found"
-- | HTTP 405 1.0 response
status405_10 = "HTTP/1.0 405 Method Not Allowed"
