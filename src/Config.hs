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

files = ["/etc/shush/shush.conf"]

type Key = String
type Value = String
type File = String
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
        putStrLn "WARNING: No config found, using defaults"
        return defaultConfig

getConfigs :: IO [Maybe Config]
getConfigs = mapM parseConfigFile files

parseConfigFile :: String -> IO (Maybe Config)
parseConfigFile file = do
    text <- tryIOError (readFile file)
    case text of
        Right config -> do
          putStrLn $ "Loaded config: " ++ file
          return $ Just $ (parseLines.lines) config
        Left err -> return Nothing

-- | Helper method to parse String into a Config
parseLines :: [String] -> Config
parseLines ls = map parseLine (filter (\x -> (not.null) x && (head x /= '#')) ls)
