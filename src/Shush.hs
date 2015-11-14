module Shush where
import Network.Socket
import qualified Data.Map.Strict as Map
import Config

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

send404_11 :: Socket -> IO Int
send404_11 sock =
    send sock $ status404_11 ++ (genHeader.length) body404 ++ "\r\n" ++ body404

sendHTTP1_0 :: Socket -> IO Int
sendHTTP1_0 sock = do
  message <- genMessage
  send sock message

genMessage :: IO String
genMessage = do
    body <- genBody
    return $ status200_10 ++ genHeader (length body) ++ "\r\n "++ body ++ "\r\n"

status200_10 = "HTTP/1.0 200 OK\r\n"
status404_10 = "HTTP/1.0 404 Not Found\r\n"
status200_11 = "HTTP/1.1 200 OK\r\n"
status404_11 = "HTTP/1.1 404 Not Found\r\n"

body404 = "<html>\r\n<head>\r\n<title>404 Not Found</title>\n\r</head>\
\\r\n<body><p><strong>404 Not Found</strong></p></body>\r\n</html>\r\n"

genHeader :: Int -> String
genHeader len = "Server: Shush/0.1\r\n" ++
    "Last-Modified: Thu, 29 Oct 2015 10:35:00 GMT\r\n" ++
    "Content-Type: text/html\r\n" ++
    "Content-Length: " ++ show len ++ "\r\n"

genBody :: IO String
genBody = readFile "index.html"

sendHTTP1_1 :: Socket -> IO Int
sendHTTP1_1 sock = undefined
