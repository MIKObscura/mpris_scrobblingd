module Stats
(
    updatePeriodStats,
    updateOverallStats
)
where
    import Data.Aeson (object, Object)
    import ScrobbleTypes (TrackInfo(..), Scrobble(..), Stats(..), StatSection(..), Configuration(..), OverallStatsSection(..))
    import Data.List (length, nub, sum, intercalate)
    import qualified Control.Monad
    import Data.Map (Map, insert, member, adjust, empty)
    import Data.List.Split
    import JsonOperations
    import Misc
    import Database.SQLite.Simple (open)
    import DBQueries

    getArtists :: [Scrobble] -> [String]
    getArtists scrobbles = nub [artist (trackInfo x) | x <- scrobbles]

    getArtistPlays :: [Scrobble] -> String -> Int
    getArtistPlays scrobbles inputArtist = length [x | x <- scrobbles, artist (trackInfo x) == inputArtist]

    getArtistPlaytime :: [Scrobble] -> String -> Integer
    getArtistPlaytime scrobbles inputArtist = sum [duration (trackInfo x) | x <- scrobbles, artist (trackInfo x) == inputArtist]

    getAlbums :: [Scrobble] -> [String]
    getAlbums scrobbles = nub [artist (trackInfo x) ++ " - " ++ album (trackInfo x) | x <- scrobbles]

    getAlbumPlays :: [Scrobble] -> String -> String -> Int
    getAlbumPlays scrobbles inputArtist inputAlbum = length [x | x <- scrobbles, (artist (trackInfo x) == inputArtist) && (album (trackInfo x) == inputAlbum)]

    getAlbumPlaytime :: [Scrobble] -> String -> String -> Integer
    getAlbumPlaytime scrobbles inputArtist inputAlbum = sum [duration (trackInfo x) | x <- scrobbles, (artist (trackInfo x) == inputArtist) && (album (trackInfo x) == inputAlbum)]

    getTracks :: [Scrobble] -> [String]
    getTracks scrobbles = nub [artist (trackInfo x) ++ " - " ++ title (trackInfo x) | x <- scrobbles]

    getTracksPlays :: [Scrobble] -> String -> String -> String -> Int
    getTracksPlays scrobbles inputArtist inputAlbum inputTitle = length [x | x <- scrobbles, (artist (trackInfo x) == inputArtist) && (album (trackInfo x) == inputAlbum) && (title (trackInfo x) == inputTitle)]

    getTotalPlaytime :: [Scrobble] -> Integer
    getTotalPlaytime scrobbles = sum [duration (trackInfo x) | x <- scrobbles]

    getWeekDayPlays :: [Scrobble] -> String -> Int
    getWeekDayPlays scrobbles weekDay = length [x | x <- scrobbles, getDayOfWeek (timestamp x) == weekDay]

    getWeekDayPlaytime :: [Scrobble] -> String -> Integer
    getWeekDayPlaytime scrobbles weekDay = sum [duration (trackInfo x) | x <- scrobbles, getDayOfWeek (timestamp x) == weekDay]

    getHODPlays :: [Scrobble] -> Int -> Int
    getHODPlays scrobbles dayTime = length [x | x <- scrobbles, getHourOfDay (timestamp x) == dayTime]

    getHODPlaytime :: [Scrobble] -> Int -> Integer
    getHODPlaytime scrobbles dayTime = sum [duration (trackInfo x) | x <- scrobbles, getHourOfDay (timestamp x) == dayTime]

    updateKey :: (Ord k, Num v) => k -> v -> Map k v -> Map k v
    updateKey k v inputMap = if member k inputMap then adjust (+ v) k inputMap
                        else insert k v inputMap

    updateArtists :: [Scrobble] -> StatSection -> [String] -> Int -> (Map String Int, Map String Integer)
    updateArtists session stats artists i = let artistPlays = getArtistPlays session (artists !! i)
                                                artistTime = getArtistPlaytime session (artists !! i)
                                                updatedArtistPlays = updateKey (artists !! i) artistPlays stats.artists_plays
                                                updatedArtistsTime = updateKey (artists !! i) artistTime stats.artists_time
                                                updatedStats = stats {artists_plays = updatedArtistPlays, artists_time = updatedArtistsTime}
                                            in
                                                if (artists !! i) == last artists then
                                                    (updatedArtistPlays, updatedArtistsTime)
                                                else
                                                    updateArtists session updatedStats artists (i + 1)

    updateAlbums :: [Scrobble] -> StatSection -> [String] -> Int -> (Map String Int, Map String Integer)
    updateAlbums session stats albums i = let parts = splitOn " - " (albums !! i)
                                              artistName = head parts
                                              albumTitle = intercalate " - " (tail parts)
                                              albumPlays = getAlbumPlays session artistName albumTitle
                                              albumTime = getAlbumPlaytime session artistName albumTitle
                                              updatedAlbumsPlays = updateKey (albums !! i) albumPlays stats.albums_plays
                                              updatedAlbumsTime = updateKey (albums !! i) albumTime stats.albums_time
                                              updatedStats = stats {albums_plays = updatedAlbumsPlays, albums_time = updatedAlbumsTime}
                                          in
                                            if (albums !! i) == last albums then
                                                (updatedAlbumsPlays, updatedAlbumsTime)
                                            else
                                                updateAlbums session updatedStats albums (i + 1)

    updateHours :: [Scrobble] -> Map Int Int -> Int -> Map Int Int
    updateHours session stats i = let item = session !! i
                                      hour = getHourOfDay item.timestamp
                                      updatedHours = updateKey hour 1 stats
                                    in
                                        if item == last session then
                                            updatedHours
                                        else
                                            updateHours session updatedHours (i + 1)

    updateDays :: [Scrobble] -> Map String Int -> Int -> Map String Int
    updateDays session stats i = let item = session !! i
                                     weekday = getDayOfWeek item.timestamp
                                     updatedDays = updateKey weekday 1 stats
                                  in
                                    if item == last session then
                                        updatedDays
                                    else
                                        updateDays session updatedDays (i + 1)

    updateOverallStats :: [Scrobble] -> Configuration -> Stats -> IO ()
    updateOverallStats session cfg stats = do
                                                conn <- open (cfg.homePath ++ "scrobble.db")
                                                topArtistsPlays <- getTopArtistsPlays conn
                                                topArtistsTime <- getTopArtistsPlaytime conn
                                                topAlbumsPlay <- getTopAlbumsPlays conn
                                                topAlbumsTime <- getTopAlbumsPlaytime conn
                                                topTracks <- getTopTracks conn
                                                overall <- getOverallStats conn
                                                let overallStats = head overall
                                                let newStats = stats { top_artists_plays = artistsToPlaysMap topArtistsPlays Data.Map.empty 0,
                                                                       top_artists_time = artistsToPlaytimeMap topArtistsTime Data.Map.empty 0,
                                                                       top_albums_plays = albumsToPlaysMap topAlbumsPlay Data.Map.empty 0,
                                                                       top_albums_time = albumsToPlaytimeMap topAlbumsTime Data.Map.empty 0,
                                                                       top_tracks = tracksToMap topTracks Data.Map.empty 0,
                                                                       total_plays = overallStats.overall_plays,
                                                                       total_listening_time = overallStats.overall_playtime,
                                                                       total_artists = overallStats.diff_artists,
                                                                       total_albums = overallStats.diff_albums,
                                                                       total_tracks = overallStats.diff_tracks,
                                                                       total_listening_days = updateDays session stats.total_listening_days 0,
                                                                       total_listening_hours = updateHours session stats.total_listening_hours 0}
                                                writeStats cfg newStats

    updatePeriodStats :: [Scrobble] -> Configuration -> Stats -> IO ()
    updatePeriodStats session cfg stats = do
        let artistsListSession = getArtists session
        let albumsListSession = getAlbums session
        Control.Monad.when (weeklyStats cfg) $ do
            weekData <- getPeriodData "week" cfg
            let artistsUpdate = updateArtists session stats.last_week artistsListSession 0
            let albumsUpdate = updateAlbums session stats.last_week albumsListSession 0
            let newWeekStats = stats.last_week { artists_plays = fst artistsUpdate,
                                            artists_time = snd artistsUpdate,
                                            albums_plays = fst albumsUpdate,
                                            albums_time = snd albumsUpdate,
                                            listening_hours = updateHours session stats.last_week.listening_hours 0,
                                            listening_days = updateDays session stats.last_week.listening_days 0,
                                            different_tracks = length (getTracks weekData),
                                            different_artists = length (getArtists weekData),
                                            different_albums = length (getAlbums weekData),
                                            total_time = getTotalPlaytime weekData}
            let newStats = stats { last_week = newWeekStats}
            writeStats cfg newStats
            newStatsFile <- getStats cfg
            Control.Monad.when (monthlyStats cfg || yearlyStats cfg) $ updatePeriodStats session (cfg { weeklyStats = False }) newStatsFile
        Control.Monad.when (monthlyStats cfg) $ do
            monthData <- getPeriodData "month" cfg
            let artistsUpdate = updateArtists session stats.last_month artistsListSession 0
            let albumsUpdate = updateAlbums session stats.last_month albumsListSession 0
            let newMonthStats = stats.last_month { artists_plays = fst artistsUpdate,
                                            artists_time = snd artistsUpdate,
                                            albums_plays = fst albumsUpdate,
                                            albums_time = snd albumsUpdate,
                                            listening_hours = updateHours session stats.last_month.listening_hours 0,
                                            listening_days = updateDays session stats.last_month.listening_days 0,
                                            different_tracks = length (getTracks monthData),
                                            different_artists = length (getArtists monthData),
                                            different_albums = length (getAlbums monthData),
                                            total_time = getTotalPlaytime monthData}
            let newStats = stats { last_month = newMonthStats}
            writeStats cfg newStats
            newStatsFile <- getStats cfg
            Control.Monad.when (yearlyStats cfg) $ updatePeriodStats session (cfg { monthlyStats = False}) newStats
        Control.Monad.when (yearlyStats cfg) $ do
            yearData <- getPeriodData "year" cfg
            let artistsUpdate = updateArtists session stats.last_year artistsListSession 0
            let albumsUpdate = updateAlbums session stats.last_year albumsListSession 0
            let newYearStats = stats.last_year { artists_plays = fst artistsUpdate,
                                            artists_time = snd artistsUpdate,
                                            albums_plays = fst albumsUpdate,
                                            albums_time = snd albumsUpdate,
                                            listening_hours = updateHours session stats.last_year.listening_hours 0,
                                            listening_days = updateDays session stats.last_year.listening_days 0,
                                            different_tracks = length (getTracks yearData),
                                            different_artists = length (getArtists yearData),
                                            different_albums = length (getAlbums yearData),
                                            total_time = getTotalPlaytime yearData}
            let newStats = stats {last_year = newYearStats}
            writeStats cfg newStats
            newStatsFile <- getStats cfg
            return ()

