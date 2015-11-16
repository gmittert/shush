{-|
Module        : Config
Description   : Functions for handling Config files
Copyright     : (c) Jason Mittertreiner, 2015
License       : GPL-3
Maintainer    : jmittert@uwaterloo.ca
Stability     : experimental
Portability   : Unix

This module contains various functions for reading and handling config files
 -}
module Config where
import Utils

file = "shush.conf"

type Key = String
type Value = String
type File = String
type Config = [(Key,Value)]

-- | Returns the Value of a Config for a given Key
getValue ::  Config -> Key -> Value
getValue config key = 
    let match = filter (\(x,_) -> key == x) config in
        (if null match then "" else (snd.head) match)

-- | Given a File name, returns a Config
parseConfigFile :: File -> IO Config
parseConfigFile file = do
    text <- readFile file
    return $ (parseLines.lines) text

-- | Helper method to parse String into a Config
parseLines :: [String] -> Config
parseLines ls = map parseLine (filter (\x -> (not.null) x && (head x /= '#')) ls)
