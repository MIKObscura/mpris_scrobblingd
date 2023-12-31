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
import System.Directory ( doesFileExist )

main :: IO ()
main = do
    envHome <- getEnv "HOME"
    let cfgPath = envHome ++ "/.config/mpris_scrobblingd/scrobbler_config.cfg"
    cfg <- readConfig cfgPath
    print cfg
    -- check if all these files exist and create them if not
    checkStats <- doesFileExist (cfg.homePath ++ "stats.json")
    checkWeek <- doesFileExist (cfg.homePath ++ "scrobble_weekly_data.json")
    checkMonth <- doesFileExist (cfg.homePath ++ "scrobble_monthly_data.json")
    checkYear <- doesFileExist (cfg.homePath ++ "scrobble_yearly_data.json")
    unless checkStats $ createEmptyStatsFile cfg
    unless checkWeek $ createEmptyPeriodFile cfg "week"
    unless checkMonth $ createEmptyPeriodFile cfg "month"
    unless checkYear $ createEmptyPeriodFile cfg "year"
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
    -- if a bus was found when the program is launched, hook to it
    unless (currentBus == "") $ do
        currentBusUname <- getUniqueBusName currentBus client
        print currentBusUname
        initialQuery <- queryBusProps client (busName_ currentBus)
        let currentMetadata = getMetadata initialQuery
        -- start registering if the bus is playing something, do nothing and wait for signals if not
        unless (Data.Map.null currentMetadata) $ do
            forkIO $ do
                newThreadId <- myThreadId
                atomically $ writeTVar waitingThreadId newThreadId
                currentTime <- getCurrentPOSIXTime
                let scrobble = convertMetadata currentMetadata currentTime (busName_ currentBus)
                print scrobble
                let waitTime = getWaitingTime cfg.timeToRegister (fromInteger scrobble.trackInfo.duration)
                threadDelay waitTime
                atomically $ modifyTVar currentSession (++[scrobble])
                return ()
            return ()
        let matchProps = matchAny { matchInterface = Just "org.freedesktop.DBus.Properties", matchPath = Just "/org/mpris/MediaPlayer2", matchMember = Just "PropertiesChanged", matchSender = Just (busName_ currentBusUname) }
        sigHandler <- addMatch client matchProps (\sig -> propsCallback sig cfg (busName_ currentBus) currentSession waitingThreadId mainThreadId)
        removeMatch client dummyHandler
        atomically $ writeTVar sigHandlerVar sigHandler
    -- check every 5 seconds if the current bus is active, do nothing if so, try to hook to another one if not
    forever $ do
        currentBus <- readTVarIO currentBusVar
        isCurrentBusActive <- isAnyBusActive [currentBus]
        if not isCurrentBusActive then do
            session <- readTVarIO currentSession
            -- register the session if not empty and empty it when done
            unless (Prelude.null session) $ do
                writeSession session cfg.keepPreviousSessions cfg.homePath
                writePeriodData session cfg
                stats <- getStats cfg
                updateOverallStats session cfg stats
                updatePeriodStats cfg
                atomically $ writeTVar currentSession []
            busNamesList <- getNamesList client
            let foundActiveBus = getBusName cfg.bus busNamesList
            print foundActiveBus
            atomically $ writeTVar currentBusVar foundActiveBus
            -- if a new bus is found, hook to it, if not do nothing
            unless (foundActiveBus == "") $ do
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
