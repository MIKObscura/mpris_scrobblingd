-- this module contains all the functions that interact with the JSON files
module JsonOperations
(
    getPeriodData,
    writeStats,
    getStats,
    writeSession,
    writePeriodData,
    createEmptyStatsFile
) where
    import Data.Aeson
    import Data.Aeson.Encode.Pretty
    import GHC.Generics
    import Data.Time.Clock.POSIX (getPOSIXTime)
    import Data.ByteString.Lazy as BS
    import qualified Data.ByteString as B
    import Data.Maybe
    import ScrobbleTypes
    import qualified Control.Monad
    import Misc
    import qualified Data.Map

    -- append the given artist name, album title, duration and timestamp (POSIX time) to the session
    addToSession :: [Scrobble] -> String -> String -> String -> Integer -> Integer -> [Scrobble]
    addToSession session artist album title duration timestamp = let newTrack = TrackInfo {artist = artist, album = album, title = title, duration = duration}
                                                                     scrobble = Scrobble {timestamp = timestamp, trackInfo = newTrack}
                                                                 in session ++ [scrobble]

    -- write the session to a JSON file
    -- if the program is configured to keep previous sessions, it will be written in a new file, otherwise the same file will be overwritten
    writeSession :: [Scrobble] -> Bool -> String -> IO ()
    writeSession session keepSession path = do
        currentTime <- getCurrentPOSIXTime
        if keepSession then
            BS.writeFile (path ++ "scrobble_session_" ++ show currentTime ++ ".json") (encodePretty session)
        else
            BS.writeFile (path ++ "scrobble_session.json") (encodePretty session)

    -- get the weekly/monthly/yearly data from the corresponding JSON file
    getPeriodData :: String -> Configuration -> IO [Scrobble]
    getPeriodData period cfg = do
        scrobbleData <- B.readFile (homePath cfg ++ "scrobble_" ++ period ++ "ly_data.json")
        return (fromJust (decodeStrict scrobbleData :: Maybe [Scrobble]))

    -- overwrite the stats file's content with the given Stat data
    writeStats :: Configuration -> Stats -> IO ()
    writeStats cfg stats = do
        BS.writeFile (homePath cfg ++ "stats.json") (encodePretty stats)

    -- create a stats file with empty data
    -- this is used for when said file doesn't exist yet
    createEmptyStatsFile :: Configuration -> IO ()
    createEmptyStatsFile cfg = do
        let emptyStats = Stats {
            total_albums = 0,
            total_listening_days = Data.Map.fromList [("Sunday", 0), ("Monday", 0), ("Tuesday", 0), ("Wednesday", 0), ("Thursday", 0), ("Friday", 0), ("Saturday", 0)],
            total_listening_hours = Data.Map.fromList [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0), (8,0), (9,0), (10,0), (11,0), (12,0), (13,0), (14,0), (15,0), (16,0), (17,0), (18,0), (19,0), (20,0), (21,0), (22,0), (23,0)],
            total_listening_time = 0,
            total_plays = 0,
            top_artists_plays = Data.Map.empty,
            top_albums_plays = Data.Map.empty,
            top_artists_time = Data.Map.empty,
            top_albums_time = Data.Map.empty,
            top_tracks = Data.Map.empty,
            total_artists = 0,
            total_tracks = 0,
            last_week = StatSection {
                different_albums = 0,
                different_artists = 0,
                different_tracks = 0,
                total_time = 0,
                albums_time = Data.Map.empty,
                albums_plays = Data.Map.empty,
                artists_time = Data.Map.empty,
                artists_plays = Data.Map.empty,
                listening_hours = Data.Map.fromList [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0), (8,0), (9,0), (10,0), (11,0), (12,0), (13,0), (14,0), (15,0), (16,0), (17,0), (18,0), (19,0), (20,0), (21,0), (22,0), (23,0)],
                listening_days = Data.Map.fromList [("Sunday", 0), ("Monday", 0), ("Tuesday", 0), ("Wednesday", 0), ("Thursday", 0), ("Friday", 0), ("Saturday", 0)]
            },
            last_month = StatSection {
                different_albums = 0,
                different_artists = 0,
                different_tracks = 0,
                total_time = 0,
                albums_time = Data.Map.empty,
                albums_plays = Data.Map.empty,
                artists_time = Data.Map.empty,
                artists_plays = Data.Map.empty,
                listening_hours = Data.Map.fromList [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0), (8,0), (9,0), (10,0), (11,0), (12,0), (13,0), (14,0), (15,0), (16,0), (17,0), (18,0), (19,0), (20,0), (21,0), (22,0), (23,0)],
                listening_days = Data.Map.fromList [("Sunday", 0), ("Monday", 0), ("Tuesday", 0), ("Wednesday", 0), ("Thursday", 0), ("Friday", 0), ("Saturday", 0)]
            },
            last_year = StatSection {
                different_albums = 0,
                different_artists = 0,
                different_tracks = 0,
                total_time = 0,
                albums_time = Data.Map.empty,
                albums_plays = Data.Map.empty,
                artists_time = Data.Map.empty,
                artists_plays = Data.Map.empty,
                listening_hours = Data.Map.fromList [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0), (8,0), (9,0), (10,0), (11,0), (12,0), (13,0), (14,0), (15,0), (16,0), (17,0), (18,0), (19,0), (20,0), (21,0), (22,0), (23,0)],
                listening_days = Data.Map.fromList [("Sunday", 0), ("Monday", 0), ("Tuesday", 0), ("Wednesday", 0), ("Thursday", 0), ("Friday", 0), ("Saturday", 0)]
            }
        }
        BS.writeFile (cfg.homePath ++ "stats.json") (encodePretty emptyStats)

    -- get the stats from the corresponding JSON file
    getStats :: Configuration -> IO Stats
    getStats cfg = do
        file <- B.readFile (homePath cfg ++ "stats.json")
        return (fromJust (decodeStrict file :: Maybe Stats))

    -- write the last session's data to the weekly/monthly/yearly JSON files
    -- also removes all data that isn't within the time period
    writePeriodData :: [Scrobble] -> Configuration -> IO ()
    writePeriodData session cfg = do
        currentTime <- getCurrentPOSIXTime
        Control.Monad.when (weeklyStats cfg) $ do
            dataFile <- B.readFile (homePath cfg ++ "scrobble_weekly_data.json")
            let weekData = fromJust (decodeStrict dataFile :: Maybe [Scrobble])
            let newData = [x | x <- weekData, isLastWeek (timestamp x) currentTime] ++ session
            BS.writeFile (homePath cfg ++ "scrobble_weekly_data.json") (encodePretty newData)
        Control.Monad.when (monthlyStats cfg) $ do
            dataFile <- B.readFile (homePath cfg ++ "scrobble_monthly_data.json")
            let monthData = fromJust (decodeStrict dataFile :: Maybe [Scrobble])
            let newData = [x | x <- monthData, isLastMonth (timestamp x) currentTime] ++ session
            BS.writeFile (homePath cfg ++ "scrobble_monthly_data.json") (encodePretty newData)
        Control.Monad.when (yearlyStats cfg) $ do
            dataFile <- B.readFile (homePath cfg ++ "scrobble_yearly_data.json")
            let yearData = fromJust (decodeStrict dataFile :: Maybe [Scrobble])
            let newData = [x | x <- yearData, isLastYear (timestamp x) currentTime] ++ session
            BS.writeFile (homePath cfg ++ "scrobble_yearly_data.json") (encodePretty newData)

