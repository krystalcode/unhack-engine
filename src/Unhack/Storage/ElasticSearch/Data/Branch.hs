{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Data.Branch
       ( updateHeadCommits
       ) where


-- Imports.

-- External dependencies.

import Data.Aeson          ((.=), object, ToJSON)
import Data.Time           (UTCTime)
import Database.Bloodhound
import GHC.Generics        (Generic)

import qualified Data.Text as T (Text)

-- Internal dependencies.

import qualified Unhack.Data.EmCommit                    as UDEC (EmCommit)
import qualified Unhack.Storage.ElasticSearch.Config     as USEC (indexSettingsFromConfig, StorageConfig)
import qualified Unhack.Storage.ElasticSearch.Operations as USEO (bulkUpdateDocuments')


-- Public API.

{-
    @Issue(
        "Use DocId instead of T.Text everywhere for document ids"
        type="task"
        priority="low"
        labels="type system, refactoring"
    )
-}

updateHeadCommits :: USEC.StorageConfig -> [(T.Text, UDEC.EmCommit)] -> UTCTime -> IO (Reply)
updateHeadCommits storageConfig branchesIdsWithEmCommits now = USEO.bulkUpdateDocuments' storageConfig indexSettings $ patches now

    where patches now   = map (\(branchId, branchHeadCommit) -> (DocId branchId, HeadCommit branchHeadCommit now)) branchesIdsWithEmCommits
          indexSettings = USEC.indexSettingsFromConfig "branch" storageConfig


-- Functions/types for internal use.

-- Patches required for the Update API.

data Patch = HeadCommit { headCommit :: UDEC.EmCommit, updatedAt :: UTCTime }
             deriving (Show, Generic)

instance ToJSON Patch where
    toJSON (HeadCommit headCommit updatedAt) =
        object [ "headCommit" .= headCommit
               , "updatedAt"  .= updatedAt
               ]
