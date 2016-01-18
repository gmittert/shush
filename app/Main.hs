{-|
Module        : Main
Description  : Shush is a Simple HTTP server
Copyright     : (c) Jason Mittertreiner, 2015
License       : GPL-3
Maintainer    : jmittert@uwaterloo.ca
Stability     : experimental
Portability   : Unix

Shush Main module
 -}
module Main where
import Config
import Shush
import HttpRequest
import HttpResponse
import HttpBody
import Control.Concurrent
import Control.Monad
import System.IO
import System.IO.Error
import Control.Exception
import Utils
import Network.Socket hiding (send, hPutStrLn)
import Network.Socket.ByteString hiding (recv)

{-|
 - Listen on a port to respond to HTTP Requests
-}
main :: IO ()
main = do
    -- Load the config
    config <- parseConfigFiles
    -- create socket
    sock <- socket AF_INET Stream 0

    {--
        - ReuseAddr does two things: it allows you to bind to any address that
        - the socket even when a previously kill process is still in TIME_WAIT
        -
        - https://stackoverflow.com/questions/14388706/socket-options-so-reuseaddr-and-so-reuseport-how-do-they-differ-do-they-mean-t
    --}
    setSocketOption sock ReuseAddr 1

    -- listen on specified port
    bindSocket sock (SockAddrInet 80 iNADDR_ANY)
    -- allow a maximum of 1 outstanding connections
    listen sock 1
    mainLoop sock config

-- | Accept one connection and handle it
mainLoop :: Socket -> Config -> IO ()
mainLoop sock config = do
  conn <- accept sock
  forkIO $ runConn conn config
  mainLoop sock config

-- | Manages a Socket request
runConn :: (Socket, SockAddr) -> Config -> IO ()
runConn (sock, _) config = do
    mesg <- tryJust(guard.isEOFError) $ recv sock 4069
    case mesg of
      Right mesg -> do
        hPutStrLn stderr mesg
        case parseRequest mesg of
            Right req ->
                (case version req of
                    HTTP_10 -> sendHTTP1_0
                    HTTP_11 -> sendHTTP1_1) req sock config
            Left _ -> do
                res <- createHTTP404
                send sock $ content (HttpResponse.body res)
        -- Hack, hard code a wait for the socket to finish
        -- before closing. Should be checking the socket status
        -- then closing it when it is done
        threadDelay 90000000
        tryIOError (sClose sock)
        return ()
      Left err -> return ()
