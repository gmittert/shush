module Config where

file = "shush.conf"

type Key = String
type Value = String
type File = String
data Config = Config File

parseConfig :: Config -> [(Key,Value)]
parseConfig config = [("http_version", "1.0")]

getValue :: Config -> Key -> Value
getValue conf key = snd .head $ filter (\x -> key == fst x) $ parseConfig conf
