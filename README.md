# Shush

Shush is a simple HTTP server written in Haskell

You can build and run shush with or without docker.
## Docker

### Building
Clone the repo and run
```
docker build -t shush .
```

### Running
You'll need to forward the port, and mount your html directory inside the container. By default, shush runs on port 9001, and reads files from `/var/www/html`
```
docker run -d -p 9001:9001 -v /YOUR/HTML/DIR:/var/www/html jmittert/shush
```

### Testing
Simply run
```
stack test
```


## Without Docker
Be sure to disable docker in stack.yaml. Change `enable:true` to `enable:false` in the docker section.

## Building
Requirements: stack
Stack will handle downloading the correct ghc and dependencies.
```
git clone http://github.com/jmittert/shush
stack setup
stack build
```

### Running
Clone the repo and run
```
stack install
shush
```
Shush will listen on port 9001 by default, and read from /var/www/html/index.html

### Testing
```
stack test
```

I've tested the above to work against Mac OSX Yosemite, and Arch Linux

### Example Config File
Place your config file in ~/.shush.conf or /etc/shush/shush.conf
```
# The version of HTTP to use (1.0 or 1.1)
http_version: 1.0

# The path to look for HTTP files
http_path: /var/www/html
```


## Implemented Features

From [here](https://www.jmarshall.com/easy/http/#http1.1s2), to comply with HTTP 1.1, a server must:
 - [x] require the Host: header from HTTP 1.1 clients
 - [x] accept absolute URL's in a request
 - [ ] accept requests with chunked data
 - [ ] either support persistent connections, or include the "Connection: close" header with each response
 - [ ] use the "100 Continue" response appropriately
 - [x] include the Date: header in each response
 - [ ] handle requests with If-Modified-Since: or If-Unmodified-Since: headers
 - [x] support at least the GET and HEAD methods
 - [x] support HTTP 1.0 requests
