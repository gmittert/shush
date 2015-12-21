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

-- | The Supported HTTP methods
data HTTPMethod = GET | POST | DELETE | PUT | HEAD deriving (Eq, Show)

-- | Given a string formatted as \"Key:Value\" returns (\"Key\",\"Value\")
parseLine :: String -> (String, String)
parseLine = dropSndHead.span (/= ':').strip

-- | Drops the head of the snd of a tuple if it exists
dropSndHead :: (String, String) -> (String, String)
dropSndHead (x,y)
    | null y = (x,y)
    | otherwise = (x, tail y)

-- | Removes whitespace from a string
strip :: String -> String
strip = filter (\ x -> x /= ' ' && x /= '\t' && x /= '\n' && x /= '\r')

-- | Returns the RFC822 Formatted Date
httpTime :: IO String
httpTime = do
    time <- getCurrentTime
    return (formatTime defaultTimeLocale rfc822DateFormat time)

