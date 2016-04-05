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
import qualified Unhack.Storage.ElasticSearch.Config as USEC (load)

-- Public API.

runPubSub :: Cmd -> IO ()
runPubSub cmd = do
    -- Load storage configuration.
    storageConfig <- USEC.load $ cmdStorageConfigFile cmd

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
                     , T.intercalate ", " $ map (T.pack . BS.unpack) channelsToSubscribe
                     , "."]

    R.runRedis conn $ R.pubSub (R.subscribe channelsToSubscribe) $ \msg -> do
      let channel = BS.unpack $ R.msgChannel msg
      let message = R.msgMessage msg

      print $ "Received message on channel '" ++ channel ++ "'"

      case (channel) of
          "uh_engine" -> do
              UPD.dispatch storageConfig message

          {-
              @Issue(
                  "Log an error message for unrecognised channels"
                  type="bug"
                  priority="low"
                  labels="log management"
              )
          -}
          _ -> putStrLn $ "The Redis PubSub channel '" ++ channel ++ "' is not recognised."

      return mempty


-- Functions/types for internal use.

-- The Redis PubSub channels to which the program will be listening.
{-
    @Issue(
        "Pass the channels to subscribe as a command line argument"
        type="improvement"
        priority="low"
    )
-}
channelsToSubscribe :: [BS.ByteString]
channelsToSubscribe = ["uh_engine"]
