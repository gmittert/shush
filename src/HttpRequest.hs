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

data HTTPVersion = HTTP_10 | HTTP_11
data HTTPMethod = GET | POST | DELETE | PUT

data HTTPRequest =
    HTTPRequest {  -- | The version of the request, 1.1 or 1.0
                   version :: HTTPVersion,
                   -- | The method of the request, (GET, POST, PUT, etc)
                   method  :: HTTPMethod,
                   -- | A map of header key value pairs
                   headers :: Map.Map String String,
                   -- | String representation of the request body
                   body    :: String
                }

parseRequest :: String -> Either Bool HTTPRequest
parseRequest = undefined

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

