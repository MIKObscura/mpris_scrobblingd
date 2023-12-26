-- this module contains anything that I think didn't fit in the other modules, this is temporary and all functions here will be in another module at some point
-- this module was supposed to be called Utils, which would have been a more clear name but it's already a module of another package apparently
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

    -- computes the time the thread will spend sleeping/waiting before registering a track
    getWaitingTime :: Float -> Int -> Int
    getWaitingTime waitFactor trackDuration = round ((waitFactor * fromIntegral trackDuration) * 1000000)

    -- returns the current time since epoch as an integer
    getCurrentPOSIXTime :: IO Integer
    getCurrentPOSIXTime = round <$> getPOSIXTime

    -- returns True if the date corresponding to the given POSIX time is within the last week, i.e. last 7 days
    isLastWeek :: Integer -> Integer -> Bool
    isLastWeek timestamp currTime = timestamp >= (currTime - 604800)

    -- returns True if the date corresponding to the given POSIX time is within the last month (for simplicity sake, 'last month' means last 30 days)
    isLastMonth :: Integer -> Integer -> Bool
    isLastMonth timestamp currTime = timestamp >= (currTime - 2592000)

    -- returns True if the date corresponding to the given POSIX time is within the last year (for simplicity sake, 'last year' means last 365 days)
    isLastYear :: Integer -> Integer -> Bool
    isLastYear timestamp currTime = timestamp >= (currTime - 31536000)

    -- transform a list of Artist into a Map containing their names and number of plays
    artistsToPlaysMap :: [Artist] -> Map String Int -> Int -> Map String Int
    artistsToPlaysMap artistsList res i = let item = artistsList !! i
                                    in
                                        if item == last artistsList then
                                            insert item.name item.totalPlays res
                                        else
                                            artistsToPlaysMap artistsList (insert item.name item.totalPlays res) (i+ 1)

    -- transform a list of Artist into a Map containing their names and time played in seconds
    artistsToPlaytimeMap :: [Artist] -> Map String Integer -> Int -> Map String Integer
    artistsToPlaytimeMap artistsList res i = let item = artistsList !! i
                                    in
                                        if item == last artistsList then
                                            insert item.name (toInteger item.totalPlaytime) res
                                        else
                                            artistsToPlaytimeMap artistsList (insert item.name (toInteger item.totalPlaytime) res) (i+ 1)

    -- transform a list of Album into a Map containing their artist name and title and number of plays
    albumsToPlaysMap :: [Album] -> Map String Int -> Int -> Map String Int
    albumsToPlaysMap albumsList res i = let item = albumsList !! i
                                            albumTitle = item.artist ++ " - " ++ item.title
                                        in
                                            if item == last albumsList then
                                                insert albumTitle item.totalPlays res
                                            else
                                                albumsToPlaysMap albumsList (insert albumTitle item.totalPlays res) (i + 1)

    -- transform a list of Album into a Map containing their artist name and title and time played in seconds
    albumsToPlaytimeMap :: [Album] -> Map String Integer -> Int -> Map String Integer
    albumsToPlaytimeMap albumsList res i = let item = albumsList !! i
                                               albumTitle = item.artist ++ " - " ++ item.title
                                        in
                                            if item == last albumsList then
                                                insert albumTitle (toInteger item.totalPlaytime) res
                                            else
                                                albumsToPlaytimeMap albumsList (insert albumTitle (toInteger item.totalPlaytime) res) (i + 1)

    -- transform a list of Track into a Map containing their title and artist name and number of plays
    tracksToMap :: [Track] -> Map String Int -> Int -> Map String Int
    tracksToMap tracksList res i = let item = tracksList !! i
                                       trackTitle = item.artist ++ " - " ++ item.title
                                    in
                                        if item == last tracksList then
                                            insert trackTitle item.plays res
                                        else
                                            tracksToMap tracksList (insert trackTitle item.plays res) (i + 1)

    -- returns the day of the week of a given POSIX time
    getDayOfWeek :: Integer -> String
    getDayOfWeek posixTime = let utcTime = posixSecondsToUTCTime (fromInteger posixTime)
                             in show (dayOfWeek (utctDay utcTime))

    -- returns the hour of day of a given POSIX time
    -- TODO: account for timezones, it only uses UTC time for now
    getHourOfDay :: Integer -> Int
    getHourOfDay posixTime = let utcTime = posixSecondsToUTCTime (fromInteger posixTime)
                             in todHour (localTimeOfDay (utcToLocalTime utc utcTime))