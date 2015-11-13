module Config where

file = "shush.conf"

type Key = String
type Value = String
type File = String
type Config = [(Key,Value)]

{--
 - Returns the value of a given key for a config
 --}
getValue ::  Config -> Key -> Value
getValue config key = 
    let match = filter (\(x,_) -> key == x) config in
        (if null match then "" else (snd.head) match)

parseConfigFile :: String -> IO Config
parseConfigFile file = do
    text <- readFile file
    return $ parseLines (lines text)

parseLines :: [String] -> Config
parseLines ls = map parseLine (filter (\x -> head x /= '#') ls)

parseLine :: String -> (Key, Value)
parseLine = dropSndHead.span (/= ':').strip

dropSndHead :: (Key, Value) -> (Key, Value)
dropSndHead (x,y) 
    | null y = (x,y)
    | otherwise = (x, tail y)

{--
 - Removes whitespace from a string
 --}
strip :: String -> String
strip = filter (\x -> x /= ' ' && x /= '\t' && x /= '\n' && x /= '\r')
