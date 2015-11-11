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
getValue config key = snd . head $ filter (\(x,_) -> key == x) config

parseConfigFile :: String -> IO Config
parseConfigFile file = do
    text <- readFile file
    return $ parseLines (lines text)

parseLines :: [String] -> Config
parseLines ls = map parseLine (filter (\x -> head x /= '#') ls)

parseLine :: String -> (Key, Value)
parseLine str = ((getBegin.strip) str, (getEnd.strip) str)

getBegin :: String -> String
getBegin l 
    | null l = []
    | head l == ':' = []
    | otherwise = head l : getBegin (tail l)

getEnd :: String -> String
getEnd l
    | null l = []
    | head l == ':' = tail l
    | otherwise  = getEnd $ tail l

{--
 - Removes whitespace from a string
 --}
strip :: String -> String
strip = filter (\x -> x /= ' ' && x /= '\t' && x /= '\n')
