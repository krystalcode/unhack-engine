{-# LANGUAGE OverloadedStrings #-}

module Unhack.Build.Action
       ( run
       ) where


-- Imports.

-- External dependencies.

import Data.Maybe          (fromJust, isNothing)
import Data.Time           (UTCTime)
import Database.Bloodhound

import qualified Data.Text as T (unpack)

-- Internal dependencies.

import qualified Unhack.Config                            as UC    (Action(..), ActionData(..), Config(..))
import qualified Unhack.Data.EmIssueCommit                as UDEIC (EmIssueCommit(..))
import qualified Unhack.Storage.ElasticSearch.Config      as USEC  (indexSettingsFromConfig, StorageConfig)
import qualified Unhack.Storage.ElasticSearch.Data.Commit as USEDC (get, setBuildStatus)


-- Public API.

-- Run a list of actions. The storage and repository configuration, together with the commit, are required as arguments
-- because they may be needed for the execution of the action.
run :: [UC.Action] -> USEC.StorageConfig -> UC.Config -> UDEIC.EmIssueCommit -> UTCTime -> IO ()
run actions storageConfig repoConfig commit now = mapM_ (runOne' storageConfig repoConfig commit now) actions

-- Functions/types for internal use.

-- Run an individual action.
runOne' :: USEC.StorageConfig -> UC.Config -> UDEIC.EmIssueCommit -> UTCTime -> UC.Action -> IO (Reply)
runOne' storageConfig repoConfig commit now action = runOne action storageConfig repoConfig commit now

runOne :: UC.Action -> USEC.StorageConfig -> UC.Config -> UDEIC.EmIssueCommit -> UTCTime -> IO (Reply)
runOne action storageConfig repoConfig commit now
    | actionType == "status" = runStatus actionData storageConfig commit now
    where actionType = UC.acType action
          actionData = UC.acData action

-- Run an action of type 'status'.
runStatus :: UC.ActionData -> USEC.StorageConfig -> UDEIC.EmIssueCommit -> UTCTime -> IO (Reply)
runStatus actionData storageConfig commit now = do
    -- We are passed the EmIssueCommit record version of the commit, but we need the full record in order to update the
    -- status.
    {- @Issue( "Implement updating only the 'buildStatus' field using the Update API"
               type="improvement"
               priority="low"
               labels="performance" ) -}
    maybeCommit <- USEDC.get storageConfig indexSettings commitId

    case (isNothing maybeCommit) of

        True  -> error $ "No commit found with id '" ++ (T.unpack commitId) ++ "'."

        False -> do
            let commitRecord = fromJust maybeCommit
            USEDC.setBuildStatus storageConfig indexSettings commitId commitRecord status now

    where commitId      = UDEIC._id commit
          status        = UC.adsName actionData
          indexSettings = USEC.indexSettingsFromConfig "commit" storageConfig
