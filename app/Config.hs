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

    getVal :: String -> [(String, String)] -> String
    getVal elem list =
        maybe "" snd (find (\ (x, _) -> x == elem) list)

    readConfig :: String -> IO Configuration
    readConfig filename = do
        file <- readFile filename
        let lines = splitOn "\n" file
        let splittedLines = [(head (splitOn "=" x), splitOn "=" x !! 1) | x <- lines]
        return Configuration {
            homePath = getVal "home_path" splittedLines,
            weeklyStats = getVal "weekly_stats" splittedLines == "true",
            monthlyStats = getVal "monthly_stats" splittedLines == "true",
            yearlyStats = getVal "yearly_stats" splittedLines == "true",
            keepPreviousSessions = getVal "keep_previous_sessions" splittedLines == "true",
            timeToRegister = read (getVal "time_to_register" splittedLines) :: Float,
            logLevel = getVal "log_level" splittedLines,
            scrobblingType = getVal "scrobbling_type" splittedLines,
            bus = splitOn ":" (getVal "bus" splittedLines)
        }
