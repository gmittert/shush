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
