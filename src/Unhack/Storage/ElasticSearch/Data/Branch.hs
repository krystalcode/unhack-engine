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

data Patch
    = HeadCommit
        -- We don't use Maybe in the 'headCommit' field, unlike the corresponding
        -- Branch record field. When a branch is created and it hasn't been
        -- analysed yet, there is no value to store in the 'headCommit' field of
        -- the branch yet. When a branch's head is updated, however, we would
        -- always have a head commit, otherwise we wouldn't try to update it in
        -- the first place.
        { headCommit :: UDEC.EmCommit
        , updatedAt  :: UTCTime
        }
    deriving (Show, Generic)

instance ToJSON Patch where
    toJSON (HeadCommit headCommit updatedAt) =
        object [ "headCommit" .= headCommit
               , "updatedAt"  .= updatedAt
               ]
