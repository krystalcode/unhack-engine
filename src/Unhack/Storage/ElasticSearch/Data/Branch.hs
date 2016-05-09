{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Data.Branch
       ( updateCommitsIds ) where


-- Imports.

-- External dependencies.

import Data.Aeson          ((.=), object, ToJSON)
import Database.Bloodhound
import GHC.Generics        (Generic)

import qualified Data.Text as T (Text)

-- Internal dependencies.

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

-- Update the 'commitsIds' field for a list of branches. Each update must be given as a tuple containing the id of the
-- branch and a list with the ids of the commits for this branch.
updateCommitsIds :: USEC.StorageConfig -> [(T.Text, [T.Text])] -> IO (Reply)
updateCommitsIds storageConfig branches = USEO.bulkUpdateDocuments' storageConfig indexSettings patches

    where patches       = map (\(branchId, branchCommitsIds) -> (DocId branchId, CommitsIds branchCommitsIds)) branches
          indexSettings = USEC.indexSettingsFromConfig "branch" storageConfig


-- Functions/types for internal use.

-- Patches required for the Update API.

-- Patch for updating the 'commitsIds' fiels
data Patch = CommitsIds { commitsIds :: [T.Text] }
             deriving (Show, Generic)

instance ToJSON Patch where
    toJSON (CommitsIds commitsIds) =
        object [ "commitsIds" .= commitsIds ]
