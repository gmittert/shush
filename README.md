# Shush

Shush is a simple http server written in Haskell

## Building
Requirements: cabal, ghc
```
git clone http://github.com/jmittert/shush
cabal install
```

## Runing:
```
cabal run
```
Shush will listen on port 9001 by default, and read from index.html

## Testing:
```
cabal configure --enable-tests
cabal build
cabal test
```

## Implemented Features

From [here](https://www.jmarshall.com/easy/http/#http1.1s2), to comply with HTTP 1.1, a server must:
 - [x] require the Host: header from HTTP 1.1 clients
 - [ ] accept absolute URL's in a request
 - [ ] accept requests with chunked data
 - [ ] either support persistent connections, or include the "Connection: close" header with each response
 - [ ] use the "100 Continue" response appropriately
 - [ ] include the Date: header in each response
 - [ ] handle requests with If-Modified-Since: or If-Unmodified-Since: headers
 - [ ] support at least the GET and HEAD methods
 - [x] support HTTP 1.0 requests
