-- this module contains all of the data types used in this project
module ScrobbleTypes
(
    Configuration(..),
    Scrobble(..),
    TrackInfo(..),
    Stats(..),
    StatSection(..),
    Artist(..),
    Track(..),
    Album(..),
    ArtistMin(..),
    AlbumMin(..),
    OverallStatsSection(..)
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
        weeklyStats :: Bool,
        monthlyStats :: Bool,
        yearlyStats :: Bool,
        keepPreviousSessions :: Bool,
        timeToRegister :: Float,
        logLevel :: String,
        scrobblingType :: String,
        bus :: [String]
    } deriving (Show)

    -- JSON structure for stats
    data StatSection = StatSection {
        different_tracks :: Int,
        different_artists :: Int,
        different_albums :: Int,
        total_time :: Integer,
        albums_time :: Map String Integer,
        albums_plays :: Map String Int,
        artists_time :: Map String Integer,
        artists_plays :: Map String Int,
        listening_hours :: Map Int Int,
        listening_days :: Map String Int,
        players :: Map String Int,
        players_time :: Map String Integer
    } deriving (Show)

    data Stats = Stats {
        total_listening_time :: Integer,
        total_plays :: Int,
        total_tracks :: Int,
        total_artists :: Int,
        top_artists_plays :: Map String Int,
        top_artists_time :: Map String Integer,
        total_albums :: Int,
        top_albums_plays :: Map String Int,
        top_albums_time :: Map String Integer,
        top_tracks :: Map String Int,
        total_listening_hours :: Map Int Int,
        total_listening_days :: Map String Int,
        last_week :: StatSection,
        last_month :: StatSection,
        last_year :: StatSection,
        overall_players :: Map String Int,
        overall_players_time :: Map String Integer
    } deriving (Show)

    -- SQL types
    data Artist = Artist {
        id :: Int,
        name :: String,
        totalPlays :: Int,
        totalPlaytime :: Int
    } deriving (Eq, Show)

    instance FromRow Artist where
        fromRow = Artist <$> field <*> field <*> field <*> field

    instance ToRow Artist where
        toRow (Artist id name plays playtime) = toRow (id, name, plays, playtime)

    data Album = Album {
        id :: Int,
        title :: String,
        artist :: String,
        totalPlays :: Int,
        totalPlaytime :: Int
    } deriving (Eq, Show)

    instance FromRow Album where
        fromRow = Album <$> field <*> field <*> field <*> field <*> field

    instance ToRow Album where
        toRow (Album id title artist plays playtime) = toRow (id, title, artist, plays, playtime)

    data Track = Track {
        id :: Int,
        title :: String,
        artist :: String,
        album :: String,
        plays :: Int,
        duration :: Int
    } deriving (Eq, Show)

    instance FromRow Track where
        fromRow = Track <$> field <*> field <*> field <*> field <*> field <*> field

    instance ToRow Track where
        toRow (Track id title artist album plays duration) = toRow (id, title, artist, album, plays, duration)

-- can't just get the result of a query as a list of int so I have to create these minimal datatypes for newly created data
    data ArtistMin = ArtistMin {
        artId :: Int,
        artName :: String
    } deriving (Eq, Show)

    instance FromRow ArtistMin where
        fromRow = ArtistMin <$> field <*> field

    instance ToRow ArtistMin where
        toRow (ArtistMin id name) = toRow (id, name)

    data AlbumMin = AlbumMin {
        albumId :: Int,
        albumTitle :: String,
        artistName :: String
    } deriving (Eq, Show)

    instance FromRow AlbumMin where
        fromRow = AlbumMin <$> field <*> field <*> field

    instance ToRow AlbumMin where
        toRow (AlbumMin id title name) = toRow (id, title, name)

    data OverallStatsSection = OverallStatsSection {
        overall_plays :: Int,
        overall_playtime :: Integer,
        diff_tracks :: Int,
        diff_artists :: Int,
        diff_albums :: Int
    }

    instance FromRow OverallStatsSection where
        fromRow = OverallStatsSection <$> field <*> field <*> field <*> field <*> field

    instance ToRow OverallStatsSection where
        toRow (OverallStatsSection overall_plays overall_playtime diff_tracks diff_artists diff_albums) = toRow (overall_plays, overall_playtime, diff_tracks, diff_artists, diff_albums)

    deriveJSON defaultOptions ''TrackInfo
    deriveJSON defaultOptions ''Scrobble
    deriveJSON defaultOptions ''StatSection
    deriveJSON defaultOptions ''Stats