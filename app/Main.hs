module Main where

import Config
import System.Environment
import ScrobbleTypes
import DBus
import DBus.Client
import DBus.Internal.Types
import MPRIS
import Misc
import Control.Monad (forever, when, unless)
import Control.Concurrent
import Control.Concurrent.STM
import Data.Map(null, Map)
import JsonOperations
import Stats
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as B
import Data.Maybe (isNothing)
import Data.Aeson (decodeStrict)

main :: IO ()
main = do
    envHome <- getEnv "HOME"
    let cfgPath = envHome ++ "/.config/mpris_scrobblingd/scrobbler_config.cfg"
    cfg <- readConfig cfgPath
    print cfg
    checkStats <- B.readFile (cfg.homePath ++ "stats.json")
    Control.Monad.when(isNothing (decodeStrict checkStats :: Maybe Stats)) $ do
        createEmptyStatsFile cfg
    client <- connectSession
    activeBusList <- getNamesList client
    mainThreadId <- myThreadId
    waitingThreadId <- newTVarIO mainThreadId
    currentSession <- newTVarIO [] :: IO (TVar [Scrobble])
    let matchProps = matchAny { matchInterface = Just "org.freedesktop.DBus.Properties", matchPath = Just "/org/mpris/MediaPlayer2", matchMember = Just "PropertiesChanged", matchSender = Just (busName_ "org.Freedesktop.DBus") }
    dummyHandler <- addMatch client matchProps (\sig -> return ())
    sigHandlerVar <- newTVarIO dummyHandler
    currentBusVar <- newTVarIO ""
    let currentBus = getBusName cfg.bus activeBusList
    print currentBus
    unless (currentBus == "") $ do
        currentBusUname <- getUniqueBusName currentBus client
        print currentBusUname
        initialQuery <- queryBusProps client (busName_ currentBus)
        let currentMetadata = getMetadata initialQuery
        unless (Data.Map.null currentMetadata) $ do
            forkIO $ do
                newThreadId <- myThreadId
                atomically $ writeTVar waitingThreadId newThreadId
                currentTime <- getCurrentPOSIXTime
                let scrobble = convertMetadata currentMetadata currentTime
                let waitTime = getWaitingTime cfg.timeToRegister (fromInteger scrobble.trackInfo.duration)
                threadDelay waitTime
                atomically $ modifyTVar currentSession (++[scrobble])
                return ()
            return ()
        let matchProps = matchAny { matchInterface = Just "org.freedesktop.DBus.Properties", matchPath = Just "/org/mpris/MediaPlayer2", matchMember = Just "PropertiesChanged", matchSender = Just (busName_ currentBusUname) }
        sigHandler <- addMatch client matchProps (\sig -> propsCallback sig cfg (busName_ currentBus) currentSession waitingThreadId mainThreadId)
        removeMatch client dummyHandler
        atomically $ writeTVar sigHandlerVar sigHandler
    forever $ do
        currentBus <- readTVarIO currentBusVar
        isCurrentBusActive <- isAnyBusActive [currentBus]
        if not isCurrentBusActive then do
            session <- readTVarIO currentSession
            unless (Prelude.null session) $ do
                writeSession session cfg.keepPreviousSessions cfg.homePath
                writePeriodData session cfg
                stats <- getStats cfg
                updateOverallStats session cfg stats
                updatedStats <- getStats cfg
                updatePeriodStats session cfg updatedStats
                atomically $ writeTVar currentSession []
            busNamesList <- getNamesList client
            let foundActiveBus = getBusName cfg.bus busNamesList
            print foundActiveBus
            unless (foundActiveBus == "") $ do
                atomically $ writeTVar currentBusVar foundActiveBus
                newBusUname <- getUniqueBusName foundActiveBus client
                print newBusUname
                prevHandler <- readTVarIO sigHandlerVar
                removeMatch client prevHandler
                let newMatch = matchAny { matchInterface = Just "org.freedesktop.DBus.Properties", matchPath = Just "/org/mpris/MediaPlayer2", matchMember = Just "PropertiesChanged", matchSender = Just (busName_ newBusUname) }
                newHandler <- addMatch client newMatch (\sig -> propsCallback sig cfg (busName_ foundActiveBus) currentSession waitingThreadId mainThreadId)
                atomically $ writeTVar sigHandlerVar newHandler
            threadDelay 5000000
        else do
            threadDelay 5000000
    return ()