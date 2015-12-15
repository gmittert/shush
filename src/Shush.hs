{-|
Module        : Shush
Description   : Network functions for Shush
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
import HttpRequest
import HttpResponse
import Config
import System.IO
import System.IO.Error
import Control.Exception

-- | Creates the body of a response to a URI request
createURIBody:: HTTPRequest -> Config -> IO String
createURIBody req config = do
    let http_path = getValue config "http_path"
    let fileName = http_path ++ "/" ++ uri req
    file <- tryIOError (openFile fileName ReadMode)::IO (Either IOError Handle)
    case file of
        Right handle -> do
            body <- readFile fileName
            hClose handle
            return (if method req == GET then body else "")
        Left _ -> return (if method req == GET then resp404Body else "")
        where resp404Body = "<html><head><title>404 Not Found</title></head>\
                             \<body><p><strong>404 Not Found</strong></p></body>\
                             \</html>"

-- | Creates the headers of a URI requrest
createHead :: HTTPRequest -> String -> IO String
createHead req body = undefined

-- | Sends a HTTP 1.0 response to a socket request
sendHTTP1_0 :: HTTPRequest -> Socket -> IO Int
sendHTTP1_0 req sock = do
    config <- parseConfigFile "shush.conf"
    case method req of
        GET -> do
            body <- createURIBody req config
            headers <- createHead req body
            send sock $ headers ++ "\r\n" ++ body
        HEAD ->  do
            body <- createURIBody req config
            headers <- createHead req body
            send sock headers
        _ -> do
            res <- http404_10
            send sock $ show res

-- | Send a HTTP 1.1 request
sendHTTP1_1 :: HTTPRequest -> Socket -> IO Int
sendHTTP1_1 = sendHTTP1_0
