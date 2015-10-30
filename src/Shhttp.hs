import Network.Socket

main :: IO ()
main = do
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
  bindSocket sock (SockAddrInet 80 iNADDR_ANY)
  -- allow a maximum of 1 outstanding connections
  listen sock 1
  mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
  -- accept one connection and handle it
  conn <- accept sock
  runConn conn
  mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
  mesg <- recv sock 4069
  putStrLn mesg
  send sock genMessage
  sClose sock

genMessage :: String
genMessage = genHeader (getLength genBody) ++ "\r\n "++ genBody

genHeader :: Int -> String
genHeader len = "HTTP/1.0 200 OK\r\n" ++
    "Server: Shush/0.1\r\n" ++
    "Last-Modified: Thu, 29 Oct 2015 10:35:00 GMT\r\n" ++
    "Content-Type: text/html\r\n" ++
    "Content-Length: " ++ (show len) ++ "\r\n"

genBody :: String
genBody = "<html><body>Hello World</body></html>\r\n"

getLength :: String -> Int
getLength str = length str
