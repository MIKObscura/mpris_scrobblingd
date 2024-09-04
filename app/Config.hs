-- this module handles the configuration of the program
module Config
(
    readConfig
)
where
    import GHC.Generics
    import System.Exit
    import Data.List.Split
    import Data.List
    import Data.Maybe
    import ScrobbleTypes(Configuration(..))

    -- extract the value from the config line
    getVal :: String -> [(String, String)] -> String
    getVal elem list =
        maybe "" snd (find (\ (x, _) -> x == elem) list)

    -- read the config file and converts it to a Configuration value
    readConfig :: String -> IO Configuration
    readConfig filename = do
        file <- readFile filename
        let lines = splitOn "\n" file
        let splittedLines = [(head (splitOn "=" x), splitOn "=" x !! 1) | x <- lines]
        return Configuration {
            homePath = getVal "home_path" splittedLines,
            timeToRegister = read (getVal "time_to_register" splittedLines) :: Float,
            logLevel = getVal "log_level" splittedLines,
            scrobblingType = getVal "scrobbling_type" splittedLines,
            bus = splitOn ":" (getVal "bus" splittedLines)
        }
