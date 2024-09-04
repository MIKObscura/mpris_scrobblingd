-- this module contains all the functions that interact with dbus
module MPRIS
(
    isAnyBusActive,
    getBusName,
    queryBusProps,
    getMetadata,
    propsCallback,
    getUniqueBusName,
    getNamesList,
    convertMetadata
) where
    import DBus
    import DBus.Client
    import Data.Map
    import Data.Maybe
    import Data.Either
    import ScrobbleTypes
    import Data.List (intercalate)
    import Data.Int (Int64)
    import Misc
    import Control.Concurrent (threadDelay, ThreadId, killThread, myThreadId)
    import Control.Concurrent.STM
    import DBQueries
    import Database.SQLite.Simple (open, close)
    import qualified Control.Monad

    -- returns True if any of the elements of the second list are in the first, False if not
    exists :: Eq a => [a] -> [a] -> Bool
    exists x y = or $ (==) <$> x <*> y

    -- returns the list of all bus names currently used
    getNamesList :: Client -> IO [String]
    getNamesList client = do
        reply <- call_ client (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames") {methodCallDestination = Just "org.freedesktop.DBus"}
        let Just namesList = fromVariant (head (methodReturnBody reply)) :: Maybe [String]
        return namesList

    -- returns True if any of the given bus is active
    isAnyBusActive :: [String] -> IO Bool
    isAnyBusActive busList = do
        client <- connectSession
        names <- getNamesList client
        return (exists busList names)

    -- returns the first bus from the second list found in the first list
    getBusName :: [String] -> [String]-> String
    getBusName busList activeNames = let foundBus = [x | x <- activeNames, x `elem` busList]
                                    in case foundBus of
                                        [] -> ""
                                        _ -> head foundBus

    -- query the given bus to get the Metadata
    queryBusProps :: Client -> BusName -> IO (Either MethodError Variant)
    queryBusProps client activeBus = do
        getProperty client (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" "Metadata") {methodCallDestination = Just activeBus}

    -- extract the metadata from a method return
    getMetadata :: Either MethodError Variant -> Map String Variant
    getMetadata reply = let replyValue = head (snd (partitionEithers [reply]))
                            justValue = fromVariant replyValue :: Maybe (Map String Variant)
                        in fromJust justValue

    -- convert metadata Map into a Scrobble
    convertMetadata :: Map String Variant -> Integer -> BusName -> Scrobble
    convertMetadata metadata posixTime currentBus = Scrobble {
        timestamp = posixTime,
        player = formatBusName currentBus,
        trackInfo = TrackInfo {
            artist = intercalate " & " (fromJust (fromVariant (fromJust (Data.Map.lookup "xesam:artist" metadata) :: Variant) :: Maybe [String]) :: [String]),
            album = fromJust (fromVariant (fromJust (Data.Map.lookup "xesam:album" metadata) :: Variant) :: Maybe String) :: String,
            title = fromJust (fromVariant (fromJust (Data.Map.lookup "xesam:title" metadata) :: Variant) :: Maybe String) :: String,
            duration = toInteger (fromJust (fromVariant (fromJust (Data.Map.lookup "mpris:length" metadata) :: Variant) :: Maybe Int64) :: Int64) `div` 1000000
        }
    }

    -- callback hooked to PropertiesChanged signal
    propsCallback :: Signal -> Configuration -> BusName -> TVar ThreadId -> ThreadId -> IO ()
    propsCallback sig cfg currentBus thread mainThreadId = do
        currentTime <- getCurrentPOSIXTime
        let changedProps = fromJust (fromVariant (sig.signalBody !! 1) :: Maybe (Map String Variant))
        let metadataLookup = Data.Map.lookup "Metadata" changedProps
        if isNothing metadataLookup then
            return ()
        else do
            thId <- readTVarIO thread
            Control.Monad.when (mainThreadId /= thId) $ killThread thId
            thisThread <- myThreadId
            atomically $ writeTVar thread thisThread
            let metadata = fromVariant (fromJust metadataLookup) :: Maybe (Map String Variant)
            let scrobble = convertMetadata (fromJust metadata) currentTime currentBus
            print scrobble
            let waitTime = getWaitingTime cfg.timeToRegister (fromInteger scrobble.trackInfo.duration)
            threadDelay waitTime
            tempClient <- connectSession
            query <- queryBusProps tempClient currentBus
            let currentMetadata = getMetadata query
            Control.Monad.when (currentMetadata == fromJust metadata) $ do
                conn <- open (cfg.homePath ++ "scrobble.db")
                registerScrobble conn scrobble
                close conn
                print scrobble
                disconnect tempClient
                return ()

    -- get the unique name of a given well-known name
    -- see here for more info about that: https://dbus.freedesktop.org/doc/dbus-specification.html#message-bus-names
    getUniqueBusName :: String -> Client -> IO String
    getUniqueBusName wellKnownName client = do
        reply <- call_ client (methodCall "/" "org.freedesktop.DBus" "GetNameOwner") {methodCallDestination = Just "org.freedesktop.DBus", methodCallBody = [toVariant wellKnownName]}
        let Just replyBody = fromVariant (head (methodReturnBody reply))
        return replyBody