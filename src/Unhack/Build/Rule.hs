{-# LANGUAGE OverloadedStrings #-}

module Unhack.Build.Rule
       ( apply
       ) where


-- Imports.

-- External dependencies.

import Data.Time (UTCTime)

-- Internal dependencies.

import qualified Unhack.Build.Action                      as UBA   (run)
import qualified Unhack.Build.Condition                   as UBC   (areMet)
import qualified Unhack.Data.EmIssueCommit                as UDEIC (EmIssueCommit)
import qualified Unhack.Config                            as UC    (Action(..), Annotation(..), Build(..), Config(..), Rule(..))
import qualified Unhack.Data.IssueProperties              as UDIP  (IssueProperties)
import qualified Unhack.Storage.ElasticSearch.Config      as USEC  (indexSettingsFromConfig, StorageConfig)


-- Public API.

-- Apply a list of rules to the given list of Issues. The rules must be contained in the given repository configuration.
-- The storage and repository configuration, and the commit that the issues belong are required as arguments because
-- they may be needed for running the actions, depending on the action types and whether the conditions are met.
-- The list of Issues is given a list of IssueProperties records.
apply :: USEC.StorageConfig -> UC.Config -> UDEIC.EmIssueCommit -> [UDIP.IssueProperties] -> UTCTime -> IO ()
apply storageConfig repoConfig commit issuesProperties now = mapM_ (applyOne' storageConfig repoConfig commit issuesProperties now) rules
    where rules = UC.bRules build
          build = UC.annBuild $ UC.confAnnotations repoConfig !! 0


-- Functions/types for internal use.

-- Apply an individual rule.
applyOne' :: USEC.StorageConfig -> UC.Config -> UDEIC.EmIssueCommit -> [UDIP.IssueProperties] -> UTCTime -> UC.Rule -> IO ()
applyOne' storageConfig repoConfig commit issuesProperties now rule = applyOne rule storageConfig repoConfig commit issuesProperties now

applyOne :: UC.Rule -> USEC.StorageConfig -> UC.Config -> UDEIC.EmIssueCommit -> [UDIP.IssueProperties] -> UTCTime-> IO ()
applyOne rule storageConfig repoConfig commit issuesProperties now
    | conditionsMet == True  = UBA.run actions storageConfig repoConfig commit now
    | conditionsMet == False = return mempty
    where conditionsMet = UBC.areMet conditions operator issuesProperties
          conditions    = UC.rConditions rule
          operator      = UC.rOperator rule
          actions       = UC.rActions rule
