module Misc
(
    getWaitingTime,
    getCurrentPOSIXTime,
    isLastMonth,
    isLastWeek,
    isLastYear,
    artistsToPlaysMap,
    artistsToPlaytimeMap,
    albumsToPlaysMap,
    albumsToPlaytimeMap,
    tracksToMap,
    getHourOfDay,
    getDayOfWeek,
)
where
    import Data.Time.Clock.POSIX
    import ScrobbleTypes
    import Data.Maybe (fromJust)
    import DBus.Internal.Types (BusName(BusName))
    import Data.Map
    import Data.Time.Clock as CLK
    import Data.Time.Calendar as CAL
    import Data.Time (TimeOfDay(todHour))
    import Data.Time.LocalTime

    getWaitingTime :: Float -> Int -> Int
    getWaitingTime waitFactor trackDuration = round ((waitFactor * fromIntegral trackDuration) * 1000000)

    getCurrentPOSIXTime :: IO Integer
    getCurrentPOSIXTime = round <$> getPOSIXTime

    isLastWeek :: Integer -> Integer -> Bool
    isLastWeek timestamp currTime = timestamp >= (currTime - 604800)

    isLastMonth :: Integer -> Integer -> Bool
    isLastMonth timestamp currTime = timestamp >= (currTime - 2592000)

    isLastYear :: Integer -> Integer -> Bool
    isLastYear timestamp currTime = timestamp >= (currTime - 31536000)

    artistsToPlaysMap :: [Artist] -> Map String Int -> Int -> Map String Int
    artistsToPlaysMap artistsList res i = let item = artistsList !! i
                                    in
                                        if item == last artistsList then
                                            insert item.name item.totalPlays res
                                        else
                                            artistsToPlaysMap artistsList (insert item.name item.totalPlays res) (i+ 1)

    artistsToPlaytimeMap :: [Artist] -> Map String Integer -> Int -> Map String Integer
    artistsToPlaytimeMap artistsList res i = let item = artistsList !! i
                                    in
                                        if item == last artistsList then
                                            insert item.name (toInteger item.totalPlaytime) res
                                        else
                                            artistsToPlaytimeMap artistsList (insert item.name (toInteger item.totalPlaytime) res) (i+ 1)

    albumsToPlaysMap :: [Album] -> Map String Int -> Int -> Map String Int
    albumsToPlaysMap albumsList res i = let item = albumsList !! i
                                            albumTitle = item.artist ++ " - " ++ item.title
                                        in
                                            if item == last albumsList then
                                                insert albumTitle item.totalPlays res
                                            else
                                                albumsToPlaysMap albumsList (insert albumTitle item.totalPlays res) (i + 1)

    albumsToPlaytimeMap :: [Album] -> Map String Integer -> Int -> Map String Integer
    albumsToPlaytimeMap albumsList res i = let item = albumsList !! i
                                               albumTitle = item.artist ++ " - " ++ item.title
                                        in
                                            if item == last albumsList then
                                                insert albumTitle (toInteger item.totalPlaytime) res
                                            else
                                                albumsToPlaytimeMap albumsList (insert albumTitle (toInteger item.totalPlaytime) res) (i + 1)

    tracksToMap :: [Track] -> Map String Int -> Int -> Map String Int
    tracksToMap tracksList res i = let item = tracksList !! i
                                       trackTitle = item.artist ++ " - " ++ item.title
                                    in
                                        if item == last tracksList then
                                            insert trackTitle item.plays res
                                        else
                                            tracksToMap tracksList (insert trackTitle item.plays res) (i + 1)

    getDayOfWeek :: Integer -> String
    getDayOfWeek posixTime = let utcTime = posixSecondsToUTCTime (fromInteger posixTime)
                             in show (dayOfWeek (utctDay utcTime))

    getHourOfDay :: Integer -> Int
    getHourOfDay posixTime = let utcTime = posixSecondsToUTCTime (fromInteger posixTime)
                             in todHour (localTimeOfDay (utcToLocalTime utc utcTime))