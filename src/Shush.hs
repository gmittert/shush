module Shush where
import Network.Socket
import qualified Data.Map.Strict as Map
import Config

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
        else send404) sock
    sClose sock

{--
 - Given an HTTPRequest, validates if it is a valid HTTP 1.1
 - request
--}
validate :: String -> Bool
validate mesg = not (getHTTPVersion mesg == "1.1"
    && not (hasHost mesg))

getHTTPVersion :: String -> String
getHTTPVersion mesg = 
    let firstLine = (head.lines) mesg in
        dropWhile (/='1') $ (last.words) firstLine

-- The headers in a request should be fairly short, it's a
-- better idea to just parse them strictly
parseHeaders :: String -> Map.Map String String
parseHeaders mesg = 
    let headers = (tail.takeWhile (/= "").map Config.strip.lines) mesg in
        createMap headers

createMap :: [String] -> Map.Map String String
createMap strs = Map.fromList (map Config.parseLine strs)

hasHost :: String -> Bool
hasHost mesg = Map.member "Host" $ parseHeaders mesg

send404 :: Socket -> IO Int
send404 = undefined

sendHTTP1_0 :: Socket -> IO Int
sendHTTP1_0 sock = do
  message <- genMessage 
  send sock message

genMessage :: IO String
genMessage = do
    body <- genBody
    return $ genHeader (length body) ++ "\r\n "++ body ++ "\r\n"

genHeader :: Int -> String
genHeader len = "HTTP/1.0 200 OK\r\n" ++
    "Server: Shush/0.1\r\n" ++
    "Last-Modified: Thu, 29 Oct 2015 10:35:00 GMT\r\n" ++
    "Content-Type: text/html\r\n" ++
    "Content-Length: " ++ show len ++ "\r\n"

genBody :: IO String
genBody = readFile "index.html"

sendHTTP1_1 :: Socket -> IO Int
sendHTTP1_1 sock = undefined
