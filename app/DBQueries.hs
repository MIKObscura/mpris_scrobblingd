-- this module contains all the queries performed to the database
module DBQueries
(
    registerScrobble
)
where
    import ScrobbleTypes
    import Database.SQLite.Simple

    registerScrobble :: Connection -> Scrobble -> IO ()
    registerScrobble conn scrobble = do
        execute conn "insert into scrobbles(timestamp, player, title, artist, album, duration) values(?,?,?,?,?,?)" 
                        (scrobble.timestamp :: Integer, scrobble.player :: String, scrobble.trackInfo.title :: String, scrobble.trackInfo.artist :: String, scrobble.trackInfo.album :: String, scrobble.trackInfo.duration :: Integer)
        return ()