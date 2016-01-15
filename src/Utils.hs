{-|
Module        : Utils
Description  : Util functions for Shush
Copyright     : (c) Jason Mittertreiner, 2015
License       : GPL-3
Maintainer    : jmittert@uwaterloo.ca
Stability     : experimental
Portability   : Unix

This module contains various utility functions for various other shush modules
 -}
module Utils where
import Data.Time
import Data.Time.Format

-- | The supported HTTP methods
data HTTPMethod = GET | POST | DELETE | PUT | HEAD deriving (Eq, Show)
-- | The supported HTTP status codes
data StatusCode = HTTP200 | HTTP404 | HTTP405 deriving (Eq, Show)

-- | A filename alias
type Filename = String

-- | Given a string formatted as \"Key:Value\" returns (\"Key\",\"Value\")
parseLine :: String -> (String, String)
parseLine = dropSndHead.span (/= ':').strip
  where
    -- | Drops the head of the second of a tuple if it exists
    dropSndHead :: (String, String) -> (String, String)
    dropSndHead (x,[]) = (x,[])
    dropSndHead (x,y) = (x, tail y)

-- | Removes whitespace from a string
strip :: String -> String
strip = filter (\ x -> x /= ' ' && x /= '\t' && x /= '\n' && x /= '\r')

-- | Returns the RFC822 Formatted Date
httpTime :: IO String
httpTime = formatTime defaultTimeLocale rfc822DateFormat <$>
  getCurrentTime
