{-|
Module        : Shush
Description  : Main network and HTTP functions for Shush
Copyright     : (c) Jason Mittertreiner, 2015
License       : GPL-3
Maintainer    : jmittert@uwaterloo.ca
Stability     : experimental
Portability   : Unix

This module contains various network functions to handle HTTP request
 -}
module Shush where
import Network.Socket
import qualified Data.Map.Strict as Map
import Utils

{-|
 - Given an HTTPRequest, validates if it is a valid HTTP 1.1
 - request
-}
validate :: String -> Bool
validate mesg = not (getHTTPVersion mesg == "1.1"
    && not (hasHost mesg))

-- | Gets the HTTP versions of a HTTP request
getHTTPVersion :: String -> String
getHTTPVersion mesg = 
    let firstLine = (head.lines) mesg in
        dropWhile (/='1') $ (last.words) firstLine

-- | Gets the headers out of a request
parseHeaders :: String -> Map.Map String String
parseHeaders mesg = 
-- The headers in a request should be fairly short, it's a
-- better idea to just parse them strictly
    let headers = (tail.takeWhile (/= "").map strip.lines) mesg in
        createMap headers

-- | Creates a Map from a list of headers
createMap :: [String] -> Map.Map String String
createMap strs = Map.fromList (map parseLine strs)

-- | Returns true if a request has a Host header
hasHost :: String -> Bool
hasHost mesg = Map.member "Host" $ parseHeaders mesg

-- | Sends a 404 to a socket request
send404_11 :: Socket -> IO Int
send404_11 sock =
    send sock $ status404_11 ++ (genHeader.length) body404 ++ "\r\n" ++ body404

-- | Sends a HTTP 1.0 response to a socket request
sendHTTP1_0 :: Socket -> IO Int
sendHTTP1_0 sock = do
  message <- genMessage
  send sock message

-- | Reads a File into a message
genMessage :: IO String
genMessage = do
    body <- genBody
    return $ status200_10 ++ genHeader (length body) ++ "\r\n "++ body ++ "\r\n"

-- | HTTP 200 1.0 response
status200_10 = "HTTP/1.0 200 OK\r\n"
-- | HTTP 404 1.0 response
status404_10 = "HTTP/1.0 404 Not Found\r\n"
-- | HTTP 200 1.1 response
status200_11 = "HTTP/1.1 200 OK\r\n"
-- | HTTP 404 1.1 response
status404_11 = "HTTP/1.1 404 Not Found\r\n"

-- | 404 HTML String
body404 = "<html>\r\n<head>\r\n<title>404 Not Found</title>\n\r</head>\
\\r\n<body><p><strong>404 Not Found</strong></p></body>\r\n</html>\r\n"

-- | Generates appriate headers for a body length
genHeader :: Int -> String
genHeader len = "Server: Shush/0.1\r\n" ++
    "Last-Modified: Thu, 29 Oct 2015 10:35:00 GMT\r\n" ++
    "Content-Type: text/html\r\n" ++
    "Content-Length: " ++ show len ++ "\r\n"

-- | Reads in a html file
genBody :: IO String
genBody = readFile "index.html"

-- | Send a HTTP 1.1 request
sendHTTP1_1 :: Socket -> IO Int
sendHTTP1_1 sock = undefined
