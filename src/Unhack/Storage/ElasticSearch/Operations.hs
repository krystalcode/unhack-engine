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
       , indexDocument'
       , updateDocument'
       , bulkIndexDocuments'
       , bulkIndexIssues
       ) where

import Data.Aeson
import qualified Data.Text as T (concat, pack, Text)
import Data.Vector (fromList)
import Database.Bloodhound
import Network.HTTP.Client (defaultManagerSettings)
import Unhack.Commit
import Unhack.Issue
import qualified Unhack.Storage.ElasticSearch.Config as USC

{-
    @Issue(
        "Review the changes for automatic ID generation and submit a pull request"
        type="task"
        priority="normal"
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
    where withBH''        = withBH' config
          deleteMapping'' = deleteMapping (indexName settings)

getDocument' :: USC.StorageConfig -> USC.StorageIndexSettings -> DocId -> IO (Reply)
getDocument' config settings@(USC.StorageIndexSettings key _ _ _) docId
    | key == "repository" = withBH'' $ getDocument'' repositoryMapping docId
    | key == "branch"     = withBH'' $ getDocument'' branchMapping docId
    | key == "commit"     = withBH'' $ getDocument'' commitMapping docId
    | key == "issue"      = withBH'' $ getDocument'' issueMapping docId
    where withBH''        = withBH' config
          getDocument'' = getDocument (indexName settings)

{-
    @Issue(
        "Review default index document settings"
        type="improvement"
        priority="low"
    )
-}
indexDocument' :: (ToJSON doc) => USC.StorageConfig -> USC.StorageIndexSettings -> doc -> IO (Reply)
indexDocument' config settings@(USC.StorageIndexSettings key _ _ _) document
    | key == "repository" = withBH'' $ indexDocument'' repositoryMapping defaultIndexDocumentSettings document
    | key == "branch"     = withBH'' $ indexDocument'' branchMapping defaultIndexDocumentSettings document
    | key == "commit"     = withBH'' $ indexDocument'' commitMapping defaultIndexDocumentSettings document
    | key == "issue"      = withBH'' $ indexDocument'' issueMapping defaultIndexDocumentSettings document
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
    where withBH''        = withBH' config
          updateDocument'' = indexDocument (indexName settings)

bulkIndexDocuments' :: (ToJSON doc) => USC.StorageConfig -> USC.StorageIndexSettings -> [doc] -> IO (Reply)
bulkIndexDocuments' config settings@(USC.StorageIndexSettings key _ _ _) docs = withBH' config $ bulk (fromList ops)
    where ops = [BulkIndex index documentMapping (DocId "") (toJSON doc) | doc <- docs]
          index = indexName settings
          documentMapping = case key of "repository" -> repositoryMapping
                                        "branch"     -> branchMapping
                                        "commit"     -> commitMapping
                                        "issue"      -> issueMapping
{-
    @Issue(
        "Use the bulkIndexDocuments' function instead"
        type="task"
        priority="low"
        labels="cleanup"
    )
-}
bulkIndexIssues :: USC.StorageConfig -> USC.StorageIndexSettings -> [Issue] -> IO (Reply)
bulkIndexIssues config settings issues = withBH' config $ bulk (fromList ops)
    where ops = [BulkIndex index issueMapping (DocId "") (toJSON issue) | issue <- issues]
          index = indexName settings


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

-- Mapping for the "repository" document type.
repositoryMapping = MappingName "repository"

data RepositoryMapping = RepositoryMapping deriving (Eq, Show)

instance ToJSON RepositoryMapping where
    toJSON RepositoryMapping
        = object [ "repository"
            .= object [ "properties"
                .= object [ "activeBranches"  .= object [ "type" .= ("nested"          :: T.Text)
                                                        , "properties"
                                                            .= object [ "_id"          .= object [ "type"   .= ("string"       :: T.Text)
                                                                                                 , "index"  .= ("not_analyzed" :: T.Text) ]
                                                                      , "name"         .= object [ "type"   .= ("string"       :: T.Text)
                                                                                                 , "index"  .= ("not_analyzed" :: T.Text) ]]]
                          , "defaultBranch"   .= object [ "type"  .= ("nested"         :: T.Text)
                                                        , "properties"
                                                            .= object [ "_id"          .= object [ "type"   .= ("string"       :: T.Text)
                                                                                                 , "index"  .= ("not_analyzed" :: T.Text) ]
                                                                      , "name"         .= object [ "type"   .= ("string"       :: T.Text)
                                                                                                 , "index"  .= ("not_analyzed" :: T.Text) ]]]
                          , "headCommit"      .= object [ "type"  .= ("nested"         :: T.Text)
                                                        , "properties"
                                                            .= object [ "_id"          .= object [ "type"   .= ("string"       :: T.Text)
                                                                                                 , "index"  .= ("not_analyzed" :: T.Text) ]
                                                                      , "hash"         .= object [ "type"   .= ("string"       :: T.Text)
                                                                                                 , "index"  .= ("not_analyzed" :: T.Text) ]
                                                                      , "time"         .= object [ "type"   .= ("date"         :: T.Text)
                                                                                                 , "format" .= ("epoch_second" :: T.Text) ]
                                                                      , "buildStatus"  .= object [ "type"   .= ("string"       :: T.Text)
                                                                                                 , "index"  .= ("not_analyzed" :: T.Text) ]
                                                                      , "buildMessage" .= object [ "type"   .= ("string"       :: T.Text)
                                                                                                 , "index"  .= ("not_analyzed" :: T.Text) ]]]
                          , "isAccessible"   .= object [ "type"  .= ("boolean"      :: T.Text)
                                                       , "index" .= ("not_analyzed" :: T.Text) ]
                          , "isActive"       .= object [ "type"  .= ("boolean"      :: T.Text)
                                                       , "index" .= ("not_analyzed" :: T.Text) ]
                          , "isDeleted"      .= object [ "type"  .= ("boolean"      :: T.Text)
                                                       , "index" .= ("not_analyzed" :: T.Text) ]
                          , "isProcessed"    .= object [ "type"  .= ("boolean"      :: T.Text)
                                                       , "index" .= ("not_analyzed" :: T.Text) ]
                          , "isPrivate"      .= object [ "type"  .= ("boolean"      :: T.Text)
                                                       , "index" .= ("not_analyzed" :: T.Text) ]
                          , "name"           .= object [ "type"  .= ("string"       :: T.Text)
                                                       , "index" .= ("not_analyzed" :: T.Text) ]
                          , "picture"        .= object [ "type"  .= ("string"       :: T.Text)
                                                       , "index" .= ("not_analyzed" :: T.Text) ]
                          , "previousUrls"   .= object [ "type"  .= ("string"       :: T.Text)
                                                       , "index" .= ("not_analyzed" :: T.Text) ]
                          , "type"           .= object [ "type"  .= ("string"       :: T.Text)
                                                       , "index" .= ("not_analyzed" :: T.Text) ]
                          , "url"            .= object [ "type"  .= ("string"       :: T.Text)
                                                       , "index" .= ("not_analyzed" :: T.Text) ]
                          , "vendor"         .= object [ "type"  .= ("string"       :: T.Text)
                                                       , "index" .= ("not_analyzed" :: T.Text) ]
                          , "vendorId"       .= object [ "type"  .= ("string"       :: T.Text)
                                                       , "index" .= ("not_analyzed" :: T.Text) ]
                          , "vendorName"     .= object [ "type"  .= ("string"       :: T.Text)
                                                       , "index" .= ("not_analyzed" :: T.Text) ]
                          , "vendorUsername" .= object [ "type"  .= ("string"       :: T.Text)
                                                       , "index" .= ("not_analyzed" :: T.Text) ] ]]]


-- Mapping for the "branch" document type.
branchMapping = MappingName "branch"

data BranchMapping = BranchMapping deriving (Eq, Show)

instance ToJSON BranchMapping where
    toJSON BranchMapping
        = object [ "branch"
            .= object [ "properties"
                .= object [ "headCommit"   .= object [ "type"  .= ("nested"       :: T.Text)
                                                     , "properties"
                                                         .= object [ "_id"          .= object [ "type"   .= ("string"       :: T.Text)
                                                                                              , "index"  .= ("not_analyzed" :: T.Text) ]
                                                                   , "hash"         .= object [ "type"   .= ("string"       :: T.Text)
                                                                                              , "index"  .= ("not_analyzed" :: T.Text) ]
                                                                   , "time"         .= object [ "type"   .= ("date"         :: T.Text)
                                                                                              , "format" .= ("epoch_second" :: T.Text) ]
                                                                   , "buildStatus"  .= object [ "type"   .= ("string"       :: T.Text)
                                                                                              , "index"  .= ("not_analyzed" :: T.Text) ]
                                                                   , "buildMessage" .= object [ "type"   .= ("string"       :: T.Text)
                                                                                              , "index"  .= ("not_analyzed" :: T.Text) ]]]
                          , "isActive"     .= object [ "type"  .= ("boolean"      :: T.Text)
                                                     , "index" .= ("not_analyzed" :: T.Text) ]
                          , "name"         .= object [ "type"  .= ("string"       :: T.Text)
                                                     , "index" .= ("not_analyzed" :: T.Text) ]
                          , "repositoryId" .= object [ "type"  .= ("string"       :: T.Text)
                                                     , "index" .= ("not_analyzed" :: T.Text) ] ]]]


-- Mapping for the "commit" document type.
commitMapping = MappingName "commit"

data CommitMapping = CommitMapping deriving (Eq, Show)

instance ToJSON CommitMapping where
    toJSON CommitMapping
        = object [ "commit"
            .= object [ "properties"
                .= object [ "repositoryId" .= object [ "type"   .= ("string"       :: T.Text)
                                                     , "index"  .= ("not_analyzed" :: T.Text) ]
                          , "hash"         .= object [ "type"   .= ("string"       :: T.Text)
                                                     , "index"  .= ("not_analyzed" :: T.Text) ]
                          , "time"         .= object [ "type"   .= ("date"         :: T.Text)
                                                     , "format" .= ("epoch_second" :: T.Text) ]
                          , "buildStatus"  .= object [ "type"   .= ("string"       :: T.Text)
                                                     , "index"  .= ("not_analyzed" :: T.Text) ]
                          , "buildMessage" .= object [ "type"   .= ("string"       :: T.Text)
                                                     , "index"  .= ("not_analyzed" :: T.Text) ] ]]]


-- Mapping for the "issue" document type.
{-
    @Issue(
        "Check if we need to set the 'norms' and 'store' properties"
        type="improvement"
        priority="normal"
        labels="performance"
    )
-}
issueMapping = MappingName "issue"

data IssueMapping = IssueMapping deriving (Eq, Show)

instance ToJSON IssueMapping where
    toJSON IssueMapping
        = object [ "issue"
            .= object [ "properties"
                .= object [ "repository" .= object [ "type" .= ("nested" :: T.Text)
                                                   , "properties"
                                                       .= object [ "id"  .= object [ "type"  .= ("string"       :: T.Text)
                                                                                   , "index" .= ("not_analyzed" :: T.Text) ]
                                                                 , "url" .= object [ "type"  .= ("string"       :: T.Text)
                                                                                   , "index" .= ("not_analyzed" :: T.Text) ]]]
                          , "commit"     .= object [ "type" .= ("nested" :: T.Text)
                                                   , "properties"
                                                       .= object [ "_id"  .= object [ "type"   .= ("string"       :: T.Text)
                                                                                    , "index"  .= ("not_analyzed" :: T.Text) ]
                                                                 , "hash" .= object [ "type"   .= ("string"       :: T.Text)
                                                                                    , "index"  .= ("not_analyzed" :: T.Text) ]
                                                                 , "time" .= object [ "type"   .= ("date"         :: T.Text)
                                                                                    , "format" .= ("epoch_second" :: T.Text) ]]]
                          , "file"       .= object [ "type"  .= ("string"       :: T.Text)
                                                   , "index" .= ("not_analyzed" :: T.Text) ]
                          , "title"      .= object [ "type"  .= ("string"       :: T.Text)
                                                   , "index" .= ("analyzed"     :: T.Text) ]
                          , "type"       .= object [ "type"  .= ("string"       :: T.Text)
                                                   , "index" .= ("not_analyzed" :: T.Text) ]
                          , "priority"   .= object [ "type"  .= ("string"       :: T.Text)
                                                   , "index" .= ("not_analyzed" :: T.Text) ]
                          , "labels"     .= object [ "type"  .= ("string"       :: T.Text)
                                                   , "index" .= ("not_analyzed" :: T.Text) ] ]]]
