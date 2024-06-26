{-# LANGUAGE OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Operations
       ( createAllIndexes
       , createIndexes
       , createIndex'
       , deleteAllIndexes
       , deleteIndexes
       , deleteIndex'
       , filterIndexSettings
       , putAllMappings
       , putMappings
       , putMapping'
       , deleteAllMappings
       , deleteMappings
       , deleteMapping'
       , getDocument'
       , mgetDocuments'
       , indexDocument'
       , updateDocument'
       , deleteDocument'
       , bulkIndexDocuments'
       , bulkUpdateDocuments'
       , bulkIndexIssues
       , search'
       , searchDefaultParams
       , deleteByQuery'
       , SearchParams(..)
       ) where


-- Imports.

-- External dependencies.

import Data.Aeson
import Data.Vector         (fromList)
import Database.Bloodhound
import Network.HTTP.Client (defaultManagerSettings, responseBody)

import qualified Data.Text as T (concat, pack, Text)

-- Internal dependencies.

import Unhack.Storage.ElasticSearch.Mappings.Branch
import Unhack.Storage.ElasticSearch.Mappings.Commit
import Unhack.Storage.ElasticSearch.Mappings.Issue
import Unhack.Storage.ElasticSearch.Mappings.Project
import Unhack.Storage.ElasticSearch.Mappings.Repository

import qualified Unhack.Issue                        as UDI (Issue)
import qualified Unhack.Storage.ElasticSearch.Config as USC
import qualified Unhack.Storage.ElasticSearch.Types  as USET

{-
    @Issue(
        "Review the changes for automatic ID generation and submit a pull request"
        type="task"
        priority="low"
        labels="contributions"
    )
-}

-- Public API.

filterIndexSettings :: [USC.StorageIndexSettings] -> [T.Text] -> [USC.StorageIndexSettings]
filterIndexSettings settings validKeys = filter (\x@(USC.StorageIndexSettings key _ _ _) -> key `elem` validKeys) settings

createAllIndexes :: USC.StorageConfig -> IO ([Reply])
createAllIndexes config = createIndexes config []

createIndexes :: USC.StorageConfig -> [T.Text] -> IO ([Reply])
createIndexes config [] = mapM (createIndex' config) $ USC.indexes config
createIndexes config validKeys = mapM (createIndex' config) $ filterIndexSettings (USC.indexes config) validKeys

createIndex' :: USC.StorageConfig -> USC.StorageIndexSettings -> IO (Reply)
createIndex' config settings = withBH' config $ createIndex (indexSettings settings) (indexName settings)

deleteAllIndexes :: USC.StorageConfig -> IO ([Reply])
deleteAllIndexes config = deleteIndexes config []

deleteIndexes :: USC.StorageConfig -> [T.Text] -> IO ([Reply])
deleteIndexes config validKeys = mapM (deleteIndex' config) $ filterIndexSettings (USC.indexes config) validKeys

deleteIndex' :: USC.StorageConfig -> USC.StorageIndexSettings -> IO (Reply)
deleteIndex' config settings = withBH' config $ deleteIndex (indexName settings)

putAllMappings :: USC.StorageConfig -> IO ([Reply])
putAllMappings config = putMappings config []

putMappings :: USC.StorageConfig -> [T.Text] -> IO ([Reply])
putMappings config validKeys = mapM (putMapping' config) $ filterIndexSettings (USC.indexes config) validKeys

putMapping' :: USC.StorageConfig -> USC.StorageIndexSettings -> IO (Reply)
putMapping' config settings@(USC.StorageIndexSettings key _ _ _)
    | key == "repository" = withBH'' $ putMapping (indexName settings) repositoryMapping RepositoryMapping
    | key == "branch"     = withBH'' $ putMapping (indexName settings) branchMapping BranchMapping
    | key == "commit"     = withBH'' $ putMapping (indexName settings) commitMapping CommitMapping
    | key == "issue"      = withBH'' $ putMapping (indexName settings) issueMapping IssueMapping
    | key == "project"    = withBH'' $ putMapping (indexName settings) projectMapping ProjectMapping
    where withBH'' = withBH' config

deleteAllMappings :: USC.StorageConfig -> IO ([Reply])
deleteAllMappings config = deleteMappings config []

deleteMappings :: USC.StorageConfig -> [T.Text] -> IO ([Reply])
deleteMappings config validKeys = mapM (deleteMapping' config) $ filterIndexSettings (USC.indexes config) validKeys

deleteMapping' :: USC.StorageConfig -> USC.StorageIndexSettings -> IO (Reply)
deleteMapping' config settings@(USC.StorageIndexSettings key _ _ _)
    | key == "repository" = withBH'' $ deleteMapping'' repositoryMapping
    | key == "branch"     = withBH'' $ deleteMapping'' branchMapping
    | key == "commit"     = withBH'' $ deleteMapping'' commitMapping
    | key == "issue"      = withBH'' $ deleteMapping'' issueMapping
    | key == "project"    = withBH'' $ deleteMapping'' projectMapping
    where withBH''        = withBH' config
          deleteMapping'' = deleteMapping (indexName settings)

getDocument' :: USC.StorageConfig -> USC.StorageIndexSettings -> DocId -> IO (Reply)
getDocument' config settings@(USC.StorageIndexSettings key _ _ _) docId
    | key == "repository" = withBH'' $ getDocument'' repositoryMapping docId
    | key == "branch"     = withBH'' $ getDocument'' branchMapping docId
    | key == "commit"     = withBH'' $ getDocument'' commitMapping docId
    | key == "issue"      = withBH'' $ getDocument'' issueMapping docId
    | key == "project"    = withBH'' $ getDocument'' projectMapping docId
    where withBH''        = withBH' config
          getDocument'' = getDocument (indexName settings)

{-
    @Issue(
        "Is there a way to unwrap the results here, even if they are of mixed types?"
        type="investigation"
        priority="low"
    )
-}
mgetDocuments' :: USC.StorageConfig -> USC.StorageIndexSettings -> [DocId] -> IO (Reply)
mgetDocuments' config settings@(USC.StorageIndexSettings key _ _ _) docsIds = withBH'' $ mgetDocuments maybeIndex maybeMapping docs
    where withBH''     = withBH' config
          docs         = map (\docId -> (maybeIndex, maybeMapping, docId)) docsIds
          maybeIndex   = Just (indexName settings)
          maybeMapping = case key of
                             "branch"     -> Just branchMapping
                             "commit"     -> Just commitMapping
                             "issue"      -> Just issueMapping
                             "repository" -> Just repositoryMapping

{-
    @Issue(
        "Review default index document settings"
        type="improvement"
        priority="low"
        labels="1.0.0-beta1"
    )
-}
indexDocument' :: (ToJSON doc) => USC.StorageConfig -> USC.StorageIndexSettings -> doc -> IO (Reply)
indexDocument' config settings@(USC.StorageIndexSettings key _ _ _) document
    | key == "repository" = withBH'' $ indexDocument'' repositoryMapping defaultIndexDocumentSettings document
    | key == "branch"     = withBH'' $ indexDocument'' branchMapping defaultIndexDocumentSettings document
    | key == "commit"     = withBH'' $ indexDocument'' commitMapping defaultIndexDocumentSettings document
    | key == "issue"      = withBH'' $ indexDocument'' issueMapping defaultIndexDocumentSettings document
    | key == "project"    = withBH'' $ indexDocument'' projectMapping defaultIndexDocumentSettings document
    where withBH''        = withBH' config
          indexDocument'' = indexDocumentAutoID (indexName settings)

{-
    @Issue(
        "Provide a function that properly uses the Update API instead of
        reindexing the full document"
        type="improvement"
        priority="low"
    )
-}
updateDocument' :: (ToJSON doc) => USC.StorageConfig -> USC.StorageIndexSettings -> doc -> DocId -> IO (Reply)
updateDocument' config settings@(USC.StorageIndexSettings key _ _ _) document id
    | key == "repository" = withBH'' $ updateDocument'' repositoryMapping defaultIndexDocumentSettings document id
    | key == "branch"     = withBH'' $ updateDocument'' branchMapping defaultIndexDocumentSettings document id
    | key == "commit"     = withBH'' $ updateDocument'' commitMapping defaultIndexDocumentSettings document id
    | key == "issue"      = withBH'' $ updateDocument'' issueMapping defaultIndexDocumentSettings document id
    | key == "project"    = withBH'' $ updateDocument'' projectMapping defaultIndexDocumentSettings document id
    where withBH''        = withBH' config
          updateDocument'' = indexDocument (indexName settings)

deleteDocument' :: USC.StorageConfig -> USET.DocType -> DocId -> IO (Reply)
deleteDocument' config documentType id = withBH'' $ deleteDocument'' id
    where withBH''         = withBH' config
          deleteDocument'' = deleteDocument (USET.toIndex config documentType) (USET.toMapping documentType)

bulkIndexDocuments' :: (ToJSON doc) => USC.StorageConfig -> USC.StorageIndexSettings -> [doc] -> IO (Reply)
bulkIndexDocuments' config settings@(USC.StorageIndexSettings key _ _ _) docs = withBH' config $ bulk (fromList ops)
    where ops = [BulkIndex index documentMapping (DocId "") (toJSON doc) | doc <- docs]
          index = indexName settings
          documentMapping = case key of "repository" -> repositoryMapping
                                        "branch"     -> branchMapping
                                        "commit"     -> commitMapping
                                        "issue"      -> issueMapping
                                        "project"    -> projectMapping

bulkUpdateDocuments' :: (ToJSON patch) => USC.StorageConfig -> USC.StorageIndexSettings -> [(DocId, patch)] -> IO (Reply)
bulkUpdateDocuments' config settings@(USC.StorageIndexSettings key _ _ _) patches = withBH' config $ bulk (fromList ops)
    where ops = [BulkUpdate index documentMapping docId (toJSON docPatch) | (docId, docPatch) <- patches]
          index = indexName settings
          documentMapping = case key of "repository" -> repositoryMapping
                                        "branch"     -> branchMapping
                                        "commit"     -> commitMapping
                                        "issue"      -> issueMapping
                                        "project"    -> projectMapping

{-
    @Issue(
        "Use the bulkIndexDocuments' function instead"
        type="task"
        priority="low"
        labels="cleanup"
    )
-}
bulkIndexIssues :: USC.StorageConfig -> USC.StorageIndexSettings -> [UDI.Issue] -> IO (Reply)
bulkIndexIssues config settings issues = withBH' config $ bulk (fromList ops)
    where ops = [BulkIndex index issueMapping (DocId "") (toJSON issue) | issue <- issues]
          index = indexName settings

search' :: USC.StorageConfig -> USC.StorageIndexSettings -> Query -> SearchParams -> IO (Reply)
search' config settings query params = withBH' config $ searchByIndex index search
    where index  = indexName settings
          search = Search (Just query) Nothing Nothing Nothing Nothing False (Just $ spFrom params) (Just $ spSize params) SearchTypeQueryThenFetch Nothing Nothing

data SearchParams = SearchParams { spFrom :: From
                                 , spSize :: Size }

searchDefaultParams = SearchParams { spFrom = From 0
                                   , spSize = Size 10 }

deleteByQuery' :: USC.StorageConfig -> [USET.DocType] -> Query -> IO (Reply)
deleteByQuery' storageConfig types query = withBH' storageConfig $ deleteByQuery maybeIndices maybeMappings searchQuery
    where searchQuery   = mkSearch (Just query) Nothing
          maybeIndices  = Just $ map (USET.toIndex storageConfig) types
          maybeMappings = Just $ map USET.toMapping types

-- Functions/types for internal use.

withBH' :: USC.StorageConfig -> BH IO a -> IO a
withBH' config = withBH defaultManagerSettings $ server config

server :: USC.StorageConfig -> Server
server config = Server $ T.concat [host, ":", T.pack(show port)]
    where host = USC.host config
          port = USC.port config

indexName :: USC.StorageIndexSettings -> IndexName
indexName settings = IndexName $ USC.name settings

indexSettings :: USC.StorageIndexSettings -> IndexSettings
indexSettings settings = IndexSettings (ShardCount shardCount) (ReplicaCount replicaCount)
    where shardCount = USC.shards settings
          replicaCount = USC.replicas settings
