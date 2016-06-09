{-# LANGUAGE OverloadedStrings #-}

module Unhack.Pubsub.Dispatcher
       ( dispatch ) where


-- Imports.

-- External dependencies.

import qualified Data.ByteString.Char8 as BS (split, unpack, ByteString)
import qualified Data.List             as L  (drop)
import qualified Data.Text             as T  (pack)

-- Internal dependencies.

import qualified Unhack.Pubsub.Repository            as UPR  (analyseAll, analyseCommits, clone, updateHeads)
import qualified Unhack.Storage.ElasticSearch.Config as USEC (indexSettingsFromConfig, StorageConfig, StorageIndexSettings)


-- Public API.

-- Dispatch an incoming pubsub message to the appropriate handling function.
{-
    @Issue(
        "Log message when the action is completed instead of printing on screen"
        type="improvement"
        priority="low"
        labels="log management"
    )
    @Issue(
        "Support json-encoded pubsub messages"
        type="bug"
        priority="low"
        labels="pubsub"
    )
-}
dispatch :: USEC.StorageConfig -> BS.ByteString -> IO ()
dispatch config message = do
    let messageParts = decodeMessage message
    let action       = BS.unpack $ messageParts !! 0

    case action of

        -- A new repository has been created.
        "repositories_create" -> do
            let repositoryId = T.pack (BS.unpack $ messageParts !! 1)
            print $ "Dispatching message of type '" ++ action ++ "'"
            UPR.clone config (USEC.indexSettingsFromConfig "repository" config) repositoryId
            {-
                @Issue(
                    "Execute the remaining tasks according to the flow"
                    type="bug"
                    priority="normal"
                    labels="release"
                )
            -}
            print $ "Action of type '" ++ action ++ "' performed"

        -- Request to analyse all commits for the active branches.
        "repositories_analyse_all" -> do
            let repositoryId = T.pack (BS.unpack $ messageParts !! 1)
            print $ "Dispatching message of type '" ++ action ++ "'"
            UPR.analyseAll config (USEC.indexSettingsFromConfig "repository" config) repositoryId
            print $ "Action of type '" ++ action ++ "' performed"

        -- Request to analyse the specified commits for the specified branch.
        "repositories_analyse_commits" -> do
            let repositoryId = T.pack (BS.unpack $ messageParts !! 1)
            let commitsIds   = map (T.pack . BS.unpack) $ L.drop 2 messageParts
            print $ "Dispatching message of type '" ++ action ++ "'"
            UPR.analyseCommits config (USEC.indexSettingsFromConfig "repository" config) repositoryId commitsIds
            print $ "Action of type '" ++ action ++ "' performed"

        "repositories_update_heads" -> do
            let repositoryId = T.pack (BS.unpack $ messageParts !! 1)
            print $ "Dispatching message of type '" ++ action ++ "'"
            UPR.updateHeads config (USEC.indexSettingsFromConfig "repository" config) repositoryId
            print $ "Action of type '" ++ action ++ "' performed"
        _ -> error $ concat ["The pubsub message type '", action, "' is not recognised."]


-- Functions/types for internal use.

decodeMessage :: BS.ByteString -> [BS.ByteString]
decodeMessage message = BS.split ':' message
