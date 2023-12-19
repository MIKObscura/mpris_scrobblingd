module DBQueries
(
    getTopAlbumsPlays,
    getOverallStats,
    getTopAlbumsPlaytime,
    getTopArtistsPlays,
    getTopArtistsPlaytime,
    getTopTracks,
    addToDatabase,
    uploadSession
)
where
    import ScrobbleTypes
    import Database.SQLite.Simple
    getTopArtistsPlays :: Connection -> IO [Artist]
    getTopArtistsPlays conn = query_ conn "select artists.id, artists.name, sum(tracks.plays) as total_plays, sum(tracks.plays * tracks.duration) \
                                            \ from artists join tracks on artists.id = tracks.artist \
                                            \ group by artists.name \
                                            \ order by total_plays desc \
                                            \ limit 10" :: IO [Artist]

    getTopArtistsPlaytime :: Connection -> IO [Artist]
    getTopArtistsPlaytime conn = query_ conn "select artists.id, artists.name, sum(tracks.plays), sum(tracks.plays * tracks.duration) as total_plays \
                                                \ from artists join tracks on artists.id = tracks.artist \
                                                \ group by artists.name \
                                                \ order by total_plays desc \
                                                \ limit 10" :: IO [Artist]

    getTopAlbumsPlays :: Connection -> IO [Album]
    getTopAlbumsPlays conn = query_ conn "select albums.id, albums.title, artists.name, sum(tracks.plays) as total_plays, sum(tracks.plays * tracks.duration) \
                                            \ from albums join tracks on albums.id = tracks.album join artists on tracks.artist = artists.id \
                                            \ group by albums.title, artists.name \
                                            \ order by total_plays desc \
                                            \ limit 10" :: IO [Album]

    getTopAlbumsPlaytime :: Connection -> IO [Album]
    getTopAlbumsPlaytime conn = query_ conn "select albums.id, albums.title, artists.name, sum(tracks.plays), sum(tracks.plays * tracks.duration) as total_plays \
                                            \ from albums join tracks on albums.id = tracks.album join artists on tracks.artist = artists.id \
                                            \ group by albums.title, artists.name \
                                            \ order by total_plays desc \
                                            \ limit 10" :: IO [Album]

    getTopTracks :: Connection -> IO [Track]
    getTopTracks conn = query_ conn "select tracks.id, tracks.title, artists.name, albums.title, plays, duration \
                                    \ from tracks join artists on tracks.artist = artists.id join albums on tracks.album = albums.id \
                                    \ order by tracks.plays desc \
                                    \ limit 10" :: IO [Track]

    getOverallStats :: Connection -> IO [OverallStatsSection]
    getOverallStats conn = query_ conn "select sum(tracks.plays), \
                                        \ sum(tracks.plays * tracks.duration), \
                                        \ count(*), count(distinct artist), \
                                        \ count(distinct album) from tracks" :: IO [OverallStatsSection]

    addToDatabase :: Connection -> String -> String -> String -> Int -> IO ()
    addToDatabase conn newArtist newAlbum newTrack newDuration = do
        execute conn "insert into artists(name) values (?)" [newArtist]
        artistData <- query conn "select id, name from artists where name = ?" [newArtist :: String] :: IO [ArtistMin]
        let artistId = artId (head artistData)
        execute conn "insert into albums(title, artist) values (?,?)" (newAlbum :: String, artistId :: Int)
        albumData <- query conn "select albums.id, title, artists.name \
                                \ from albums join artists on albums.artist = artists.id \
                                \ where title = ? and artist = ?" (newAlbum :: String, artistId :: Int) :: IO [AlbumMin]
        let albumsId = albumId (head albumData)
        execute conn "insert into tracks(title, album, artist, duration, plays) values (?,?,?,?,1)" (newTrack :: String, albumsId :: Int, artistId :: Int, newDuration :: Int)
        return ()

    uploadSession :: Connection -> [Scrobble] -> Int -> IO ()
    uploadSession conn session i = do
                                        let item = session !! i
                                        if item == last session then do
                                            addToDatabase conn item.trackInfo.artist item.trackInfo.album item.trackInfo.title (fromInteger item.trackInfo.duration)
                                        else do
                                            addToDatabase conn item.trackInfo.artist item.trackInfo.album item.trackInfo.title (fromInteger item.trackInfo.duration)
                                            uploadSession conn session (i + 1)
                                        return ()