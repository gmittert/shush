{-|
Module        : HttpRequest
Description  :  HTTP functions for Shush
Copyright     : (c) Jason Mittertreiner, 2015
License       : GPL-3
Maintainer    : jmittert@uwaterloo.ca
Stability     : experimental
Portability   : Unix

This module contains various HTTPRequest related functions
 -}
module HttpRequest where
import qualified Data.Map.Strict as Map
import Utils

-- | Data type representing HTTP/1.0 or HTTP/1.1
data HTTPVersion = HTTP_10 | HTTP_11 deriving (Eq, Show)

-- | An HttpRequest datatype
data HTTPRequest =
    HTTPRequest {  -- | The version of the request, 1.1 or 1.0
                   version :: HTTPVersion,
                   -- | The method of the request, (GET, POST, PUT, etc)
                   method  :: HTTPMethod,
                   -- | String representation requested uri
                   uri :: String,
                   -- | A map of header key value pairs
                   headers :: Map.Map String String,
                   -- | String representation of the request body
                   body    :: String
                } deriving (Eq, Show)

-- | Returns a HTTPRequest from a request String
parseRequest :: String -> Either Bool HTTPRequest
parseRequest request = let
    version = getHTTPVersion request
    method = getMethod request
    uri = getRequestURI request
    headers = parseHeaders request
    body = getBody request in
        if validate request then
            Right $ HTTPRequest version method uri headers body
        else Left False

{-|
 - Given an HTTPRequest, validates if it is a valid HTTP 1.1
 - request
-}
validate :: String -> Bool
validate mesg = not (getHTTPVersion mesg == HTTP_11
    && not (hasHost mesg))

-- | Gets the HTTP versions of a HTTP request
getHTTPVersion :: String -> HTTPVersion
getHTTPVersion mesg =
    let version = dropWhile (/='1') $ (last.words) (getStatusLine mesg) in
        if version == "1.1" then HTTP_11 else HTTP_10

-- | Gets the headers out of a request
parseHeaders :: String -> Map.Map String String
parseHeaders request =
-- The headers in a request should be fairly short, it's a
-- better idea to just parse them strictly
    let headers = (tail.takeWhile (/= "").map strip.lines) request in
        createMap headers

-- | Creates a Map from a list of headers
createMap :: [String] -> Map.Map String String
createMap strs = Map.fromList (map parseLine strs)

-- | Returns the status line of a message
getStatusLine = head.lines

-- | Returns true if a request has a Host header
hasHost :: String -> Bool
hasHost mesg = Map.member "Host" $ parseHeaders mesg

-- | Returns the requested file of the request
getRequestURI :: String -> String
getRequestURI req = let uri = words req !! 1 in
    (if uri == "/" then "index.html" else uri)

-- | Returns the HTTPMethod of a request
-- | GET somefile.html HTTP/1.1\r\n\ -> GET
getMethod :: String -> HTTPMethod
getMethod response
    | method == "GET" = GET
    | method == "POST" = POST
    | method == "PUT" = PUT
    | method == "DELETE" = DELETE
    | method == "HEAD" = HEAD
    where method = (head.words.getStatusLine) response

-- | Returns true if a line is a newline
isNewline:: String -> Bool
isNewline str = str == "" || str == "\r"

-- | Returns the body of a request
getBody :: String -> String
getBody = unlines.(\x -> if null x then x else tail x).dropWhile (not.isNewline).lines
