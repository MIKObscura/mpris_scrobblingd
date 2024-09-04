-- this module contains all of the data types used in this project
module ScrobbleTypes
(
    Configuration(..),
    Scrobble(..),
    TrackInfo(..)
)
where
    import Data.Aeson
    import Data.Aeson.TH (deriveJSON, defaultOptions)
    import Data.Map
    import Database.SQLite.Simple

    -- JSON structure for session/period data
    data TrackInfo = TrackInfo {
        artist :: String,
        album :: String,
        title :: String,
        duration :: Integer
    } deriving (Show, Eq)

    data Scrobble = Scrobble {
        timestamp :: Integer,
        player :: String,
        trackInfo :: TrackInfo
    } deriving (Show, Eq)

    -- program's configuration
    data Configuration = Configuration {
        homePath :: String,
        timeToRegister :: Float,
        logLevel :: String,
        scrobblingType :: String,
        bus :: [String]
    } deriving (Show)