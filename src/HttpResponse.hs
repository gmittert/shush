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
                     http404_11,
                     http404_10,
                     status200_11,
                     status200_10,
                     http404Body,
                     status,
                     body,
                     headers,
                     headerStr,
                     formatHeaders
        ) where
import qualified Data.Map.Strict as Map
import Utils

data HTTPResponse =
    HTTPResponse { -- | The method of the request, (GET, POST, PUT, etc)
                   status :: String,
                   -- | String representation requested uri
                   headers :: Map.Map String String,
                   -- | String representation of the request body
                   body    :: String
                } deriving (Eq)

instance Show HTTPResponse where
    show (HTTPResponse status headers body) =
        status ++ "\r\n" ++
        formatHeaders headers ++
        "\r\n" ++
        body

-- | Creates an HTTPResponse out of a status line and body
createHTTPResponse :: String -> String -> IO HTTPResponse
createHTTPResponse status body = do
    time <- httpTime
    let headers = genHeader ((show.length) body) time in
      return $ HTTPResponse status headers body

-- | Generates appriate headers for a body length and time
genHeader :: String -> String -> Map.Map String String
genHeader len time =
    Map.fromList [("Server", "Shush/0.1"),
        ("Last-Modified", time),
        ("Content-Type", "text/html"),
        ("Date", time),
        ("Content-Length", len)]

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
-- | Returns a HTTP 404 HTTPResponse for HTTP/1.1
http404_11 = http404 "HTTP/1.1 404 Not Found"
-- | Returns a HTTP 404 HTTPResponse for HTTP/1.0
http404_10 = http404 "HTTP/1.0 404 Not Found"

-- | Returns a HTTP 404 HTTPResponse for a given status
http404 status = createHTTPResponse status http404Body

-- | Returns a body for a HTTP 404 response
http404Body = "<html><head><title>404 Not Found</title></head>" ++
    "<body><p><strong>404 Not Found</strong></p></body></html>\r\n"
