{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Data.Commit
       ( bulkIndex
       , bulkUpdateBranches
       , get
       , mget
       , setBuildStatus )
       where


-- Imports.

-- External dependencies.

import GHC.Generics        (Generic)
import Data.Aeson          ((.=), eitherDecode, object, ToJSON)
import Database.Bloodhound
import Network.HTTP.Client

import qualified Data.Map  as M (map, toList, Map)
import qualified Data.Text as T (Text)

-- Internal dependencies.

import qualified Unhack.Commit                           as UDC  (Commit(..))
import qualified Unhack.Data.EmBranch                    as UDEB (EmBranch(..))
import qualified Unhack.Storage.ElasticSearch.Config     as USEC (indexSettingsFromConfig, StorageConfig, StorageIndexSettings)
import qualified Unhack.Storage.ElasticSearch.Operations as USEO (bulkIndexDocuments', bulkUpdateDocuments', getDocument', mgetDocuments', updateDocument')


-- Public API.

-- Bulk index new commits.
{-
    @Issue(
        "Go through the response and log an error if not all Commits were
        properly stored"
        type="bug"
        priority="normal"
        labels="error management, log management"
    )
-}
bulkIndex :: USEC.StorageConfig -> [UDC.Commit] -> IO ([T.Text])
bulkIndex _ [] = return []
bulkIndex storageConfig commits = do
    bulkEsResult <- USEO.bulkIndexDocuments' storageConfig indexSettings commits
    print bulkEsResult

    let body             = responseBody bulkEsResult
    let eitherResult     = eitherDecode body :: Either String BulkEsResult
    let result           = either error id eitherResult
    let resultItems      = berItems result
    let resultItemsInner = map bericCreate resultItems
    let lNewCommitsIds   = map berId resultItemsInner

    return lNewCommitsIds

    where indexSettings = USEC.indexSettingsFromConfig "commit" storageConfig

-- Get a commit record given its ID.
get :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> IO (Maybe UDC.Commit)
get config indexSettings commitId = do
    -- Get the commit from Elastic Search.
    response <- USEO.getDocument' config indexSettings (DocId commitId)

    -- Get the result from the body of the response.
    let body         = responseBody response
    let eitherResult = eitherDecode body :: Either String (EsResult UDC.Commit)
    let result       = either error id eitherResult

    -- Get the commit from the _source attribute of the result.
    let maybeCommit = fmap _source . foundResult $ result

    return maybeCommit

mget :: USEC.StorageConfig -> [T.Text] -> IO ([Maybe UDC.Commit])
mget storageConfig commitsIds = do
    response <- USEO.mgetDocuments' storageConfig indexSettings docsIds

    let body         = responseBody response
    let eitherResult = eitherDecode body :: Either String (MgetEsResult UDC.Commit)
    let result       = either error id eitherResult

    return $ map (fmap _source . foundResult) $ mgetDocs result

    where indexSettings = USEC.indexSettingsFromConfig "commit" storageConfig
          docsIds       = map DocId commitsIds

-- Set the 'buildStatus' for a commit to the given value.
setBuildStatus :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> UDC.Commit -> T.Text -> IO (Reply)
setBuildStatus config indexSettings commitId commit buildStatus = USEO.updateDocument' config indexSettings updatedCommit (DocId commitId)
    where updatedCommit = commit { UDC.buildStatus = buildStatus }
bulkUpdateBranches :: USEC.StorageConfig -> M.Map DocId (Maybe [UDEB.EmBranch]) -> IO (Reply)
bulkUpdateBranches storageConfig mCommitsEmBranchesWithIds = USEO.bulkUpdateDocuments' storageConfig indexSettings patches

    where patches       = M.toList $ M.map (\emBranches -> Branches emBranches) mCommitsEmBranchesWithIds
          indexSettings = USEC.indexSettingsFromConfig "commit" storageConfig


-- Functions/types for internal use.

-- Patches required for the Update API.

data Patch = Branches    { branches    :: Maybe [UDEB.EmBranch] }
             deriving (Show, Generic)

instance ToJSON Patch where
    toJSON (Branches branches) =
        object [ "branches" .= branches ]
