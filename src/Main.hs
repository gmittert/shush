module Main where
import Config
import Shush
import Network.Socket

main :: IO ()
main = do
    config <- parseConfigFile "shush.conf"
    -- create socket
    sock <- socket AF_INET Stream 0

    {--
        - ReuseAddr does two things: it allows you to bind to any address that
        - is not in use by that exact address and port. e.g. both
        - 192.168.0.1:80 and 0.0.0.0:80, additionally, it will allow rebinding to
        - the socket even when a previously kill process is still in TIME_WAIT
        -
        - https://stackoverflow.com/questions/14388706/socket-options-so-reuseaddr-and-so-reuseport-how-do-they-differ-do-they-mean-t
    --}
    setSocketOption sock ReuseAddr 1

    -- listen on TCP port 80
    bindSocket sock (SockAddrInet 9001 iNADDR_ANY)
    -- allow a maximum of 1 outstanding connections
    listen sock 1
    mainLoop sock $ getValue config "http_version"

mainLoop :: Socket -> String -> IO ()
mainLoop sock httpVersion = do
  -- accept one connection and handle it
  conn <- accept sock
  runConn conn httpVersion
  mainLoop sock httpVersion

runConn :: (Socket, SockAddr) -> String -> IO ()
runConn (sock, _) httpVersion = do
    mesg <- recv sock 4069
    putStrLn mesg
    (if validate mesg then
        if httpVersion == "1.0" then sendHTTP1_0 else sendHTTP1_1
        else send404_11) sock
    sClose sock
