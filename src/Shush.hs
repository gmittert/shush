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
import qualified Network.Socket.ByteString as NSB
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Utils
import HttpRequest
import HttpResponse
import Config
import System.IO
import System.IO.Error
import Control.Exception
import HttpBody

-- | Sends a HTTP 1.0 response to a socket request
sendHTTP1_0 :: HTTPRequest -> Socket -> IO Int
sendHTTP1_0 req sock = do
    config <- parseConfigFiles
    reqbody <- bodyFromRequest req config
    res <- createHTTPResponse reqbody
    send sock $ formatMetadata res
    NSB.send sock $ content (HttpResponse.body res)

-- | Send a HTTP 1.1 request
sendHTTP1_1 :: HTTPRequest -> Socket -> IO Int
sendHTTP1_1 = sendHTTP1_0
