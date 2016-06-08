{-# LANGUAGE OverloadedStrings #-}

module Unhack.Pubsub.Publish
       ( publish ) where


-- Imports.

-- External dependencies.

import Data.Maybe (fromJust)

import qualified Data.ByteString.Char8 as BS (pack, ByteString)
import qualified Data.Text             as T  (unpack, Text)
import qualified Database.Redis        as R  (connect, connectHost, defaultConnectInfo, publish, runRedis, Reply)

-- Internal depdendencies.

import qualified Unhack.Pubsub.Router as UPR (route)


-- Public API.

-- Publish a list of messages.
publish :: [(T.Text, T.Text)] -> IO [(Either R.Reply Integer)]
publish messages = mapM publishOne messages


-- Functions/types for internal use.

-- Publish a message. The repository ID that the message relates to is used to
-- determine which pubsub channel the message will be send to.
publishOne :: (T.Text, T.Text) -> IO (Either R.Reply Integer)
publishOne (repositoryId, message) = do
    {-
        @Issue(
          "Handle error if no channel is returned"
            type="bug"
            priority="low"
            labels="error management"
        )
    -}
    maybeChannel <- UPR.route repositoryId
    redisConnection <- R.connect $ R.defaultConnectInfo { R.connectHost = "redis" }
    R.runRedis redisConnection $ R.publish (fromJust maybeChannel) bsMessage

    where bsMessage = BS.pack $ T.unpack message
