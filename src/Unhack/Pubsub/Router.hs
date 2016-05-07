{-# LANGUAGE OverloadedStrings #-}

module Unhack.Pubsub.Router
       ( getChannel
       , route ) where


-- Imports.

import Data.Binary        (decode, encode)
import Data.Maybe         (fromJust, isNothing)
import System.Environment (getEnv)
import System.Random      (getStdGen, randomR)

import qualified Data.ByteString.Char8 as BS  (append, pack, ByteString)
import qualified Data.ByteString.Lazy  as BSL (fromStrict, toStrict)
import qualified Data.Text             as T   (concat, pack, unpack, Text)
import qualified Database.Redis        as R   (connect, connectHost, defaultConnectInfo, get, runRedis, set)


-- Public API.

-- Returns the channel name for the current process.
getChannel :: IO (BS.ByteString)
getChannel = do
    channelEnv <- getEnv "UNHACK_ENGINE_INSTANCE"
    return $ channelName (read channelEnv :: Int)

-- Routes a repository to a pubsub channel.
-- It works by requesting to Redis whether a repository/channel assignment already exists, and
-- returns that value if so. If it doesn't exist, it assigns a random channel to the repository.
-- The assignment channels are stored in Redis as integer numbers that correspond to one process
-- (worker). The route (channel name) is constructed by prefixing a default string to this number.
-- This prevents from mixing our channels with other possible pubsub channel schemes that might run
-- on the same Redis cluster.
route :: T.Text -> IO (Maybe BS.ByteString)
route repositoryId = do
    let repositoryRouteKey = routeKey (BS.pack $ T.unpack repositoryId)

    -- Check if there is already an assignment stored in Redis.
    redisConnection  <- R.connect $ R.defaultConnectInfo { R.connectHost = "redis" }
    eitherAssignment <- R.runRedis redisConnection $ R.get repositoryRouteKey
    let maybeAssignment = either (error . show) id eitherAssignment

    case (isNothing maybeAssignment) of

        -- If no, create one, store it in Redis and return it.
        True  -> do
            g <- getStdGen
            nbChannels <- getNbChannels
            let (newAssignment, _) = randomR (1, nbChannels) g
            let bsNewAssignment = BSL.toStrict $ encode (newAssignment :: Int)

            {-
              @Issue (
                "Consider sending it to Redis asynchronously"
                type="improvement"
                priority="low"
                labels="performance"
              )
              @Issue (
                "Log if we have an error and return Nothing"
                type="bug"
                priority="low"
                labels="error management, log management"
              )
            -}
            eitherNewAssignment <- R.runRedis redisConnection $ R.set repositoryRouteKey bsNewAssignment
            let maybeNewAssignment = either (error . show) id eitherNewAssignment

            -- Convert the assignment to the corresonding channel name and
            -- return that.
            return $ Just (channelName newAssignment)

        -- If there is an existing assignment, convert it to the corresponding
        -- channel name and return that.
        False -> do
            let assignment = BSL.fromStrict $ fromJust maybeAssignment
            let intAssignment = decode assignment
            return $ Just (channelName intAssignment)


-- Functions/types for internal use.

-- Prepares the Redis key where the channels corresponding to the given
-- repository will be stored.
routeKey :: BS.ByteString -> BS.ByteString
routeKey repositoryId = BS.append (BS.pack "psc") repositoryId

-- Returns the total number of channels that corresponds to the total number of
-- processes (workers) across all infrastructure.
getNbChannels :: IO (Int)
getNbChannels = do
    nbChannelsEnv <- getEnv "UNHACK_ENGINE_NB_INSTANCES"
    return (read nbChannelsEnv :: Int)

-- Applies the channel prefix to the channel number so that we get the final channel name.
channelName :: Int -> BS.ByteString
channelName channelNumber = BS.append channelPrefix bsChannelNumber
    where bsChannelNumber = BS.pack $ show channelNumber

-- The prefix prepended on channels before the channel number, e.g. uh_engine_1, uh_engine_2 etc.
channelPrefix :: BS.ByteString
channelPrefix = BS.pack "uh_engine_"
