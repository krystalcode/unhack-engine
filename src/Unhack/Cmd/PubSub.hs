{-# LANGUAGE OverloadedStrings #-}

module Unhack.Cmd.PubSub
       ( runPubSub
       ) where


-- Imports.

import qualified Data.ByteString.Char8 as BS (unpack, ByteString)
import qualified Data.Text             as T  (concat, intercalate, pack)
import qualified Database.Redis        as R  (connect, connectHost, defaultConnectInfo, msgChannel, msgMessage, pubSub, runRedis, subscribe)

import           Unhack.Cmd.Modes
import qualified Unhack.Pubsub.Dispatcher            as UPD  (dispatch)
import qualified Unhack.Pubsub.Router                as UPR  (getChannel)
import qualified Unhack.Storage.ElasticSearch.Config as USEC (load)

-- Public API.

runPubSub :: Cmd -> IO ()
runPubSub cmd = do
    -- Load storage configuration.
    storageConfig <- USEC.load $ cmdStorageConfigFile cmd

    -- Get the channel that we will be listening to.
    subscribedChannel <- UPR.getChannel

    -- Initiate a connection to Redis.
    {-
        @Issue(
            "Load the redis connection details from a configuration file"
            type="improvement"
            priority="low"
            labels="configuration management"
        )
    -}
    conn <- R.connect $ R.defaultConnectInfo { R.connectHost = "redis" }

    -- Listen to Redis PubSub channels for messages and dispatch them
    -- accordingly.
    print $ T.concat [ "Listening to the following Redis PubSub channels: "
                     , T.intercalate ", " $ map (T.pack . BS.unpack) [subscribedChannel]
                     , "."]

    R.runRedis conn $ R.pubSub (R.subscribe [subscribedChannel]) $ \msg -> do
      let channel = R.msgChannel msg
      let message = R.msgMessage msg

      print $ "Received message on channel '" ++ (BS.unpack channel) ++ "'"

      case (channel == subscribedChannel) of
          True -> do
              UPD.dispatch storageConfig message

          {-
              @Issue(
                  "Log an error message for unrecognised channels"
                  type="bug"
                  priority="low"
                  labels="log management"
              )
              @Issue(
                  "Is there really any case where we would received messages on unrecognised channels?"
                  type="investigation"
                  priority="low"
                  labels="clean up"
              )
          -}
          False -> putStrLn $ "The Redis PubSub channel '" ++ (BS.unpack channel) ++ "' is not recognised."

      return mempty
