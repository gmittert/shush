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
import System.IO
import System.IO.Error
import Control.Exception
import Data.Monoid

-- | The list of possible locations for the config file
files = ["/etc/shush/shush.conf"]

-- | Key type
type Key = String
-- | Value type
type Value = String
-- | A Config is a list of String String tuples
type Config = [(Key,Value)]

-- | Returns the Value of a Config for a given Key
getValue ::  Config -> Key -> Value
getValue config key =
    let match = filter (\(x,_) -> key == x) config in
      (if null match then "" else (snd.head) match)

-- | Returns a default config
defaultConfig :: Config
defaultConfig = [("http_version","1.0"),("http_path","/var/www/html")]

-- | Given a File name, returns a Config
parseConfigFiles :: IO Config
parseConfigFiles = do
    configs <- getConfigs
    case getFirst . mconcat . map First $ configs of
      Just a -> return a
      Nothing -> do
        hPutStrLn stderr "WARNING: No config found, using defaults"
        return defaultConfig

-- | Returns a list of possible config files
getConfigs :: IO [Maybe Config]
getConfigs = mapM parseConfigFile files

-- | Given a file name, parses it into a config
parseConfigFile :: Filename -> IO (Maybe Config)
parseConfigFile file = do
    text <- tryIOError (readFile file)
    case text of
        Right config -> do
          hPutStrLn stderr $ "Loaded config: " ++ file
          return $ Just $ (parseLines.lines) config
        Left err -> return Nothing

-- | Helper method to parse a lines into a Config
parseLines :: [String] -> Config
parseLines ls = map parseLine (filter (\x -> (not.null) x && (head x /= '#')) ls)
