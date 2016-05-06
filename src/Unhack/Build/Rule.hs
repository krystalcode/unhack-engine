{-# LANGUAGE OverloadedStrings #-}

module Unhack.Build.Rule
       ( apply
       ) where


-- Imports.

import qualified Unhack.Build.Action                      as UBA   (run)
import qualified Unhack.Build.Condition                   as UBC   (areMet)
import qualified Unhack.Data.EmIssueCommit                as UDEIC (EmIssueCommit)
import qualified Unhack.Config                            as UC    (Action(..), Annotation(..), Build(..), Config(..), Rule(..))
import qualified Unhack.Issue                             as UDI   (Issue)
import qualified Unhack.Storage.ElasticSearch.Config      as USEC  (indexSettingsFromConfig, StorageConfig)
import qualified Unhack.Storage.ElasticSearch.Data.Commit as USEDC (setBuildStatus)


-- Public API.

-- Apply a list of rules to the given list of Issues. The rules must be contained in the given repository configuration.
-- The storage and repository configuration, and the commit that the issues belong are required as arguments because
-- they may be needed for running the actions, depending on the action types and whether the conditions are met.
apply :: USEC.StorageConfig -> UC.Config -> UDEIC.EmIssueCommit -> [UDI.Issue] ->  IO ()
apply storageConfig repoConfig commit issues = mapM_ (applyOne' storageConfig repoConfig commit issues) rules
    where rules = UC.bRules build
          build = UC.annBuild $ UC.confAnnotations repoConfig !! 0


-- Functions/types for internal use.

-- Apply an individual rule.
applyOne' :: USEC.StorageConfig -> UC.Config -> UDEIC.EmIssueCommit -> [UDI.Issue] -> UC.Rule ->  IO ()
applyOne' storageConfig repoConfig commit issues rule = applyOne rule storageConfig repoConfig commit issues

applyOne :: UC.Rule -> USEC.StorageConfig -> UC.Config -> UDEIC.EmIssueCommit -> [UDI.Issue] ->  IO ()
applyOne rule storageConfig repoConfig commit issues
    | conditionsMet == True  = UBA.run actions storageConfig repoConfig commit
    | conditionsMet == False = return mempty
    where conditionsMet = UBC.areMet conditions operator issues
          conditions    = UC.rConditions rule
          operator      = UC.rOperator rule
          actions       = UC.rActions rule
