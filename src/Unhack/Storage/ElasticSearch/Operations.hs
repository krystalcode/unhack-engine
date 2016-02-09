{-# LANGUAGE OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Operations
       ( createIndexes
       , createIndex'
       , deleteIndexes
       , deleteIndex'
       , putMappings
       , putMapping'
       , deleteMappings
       , deleteMapping'
       , indexDocument'
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

createIndexes :: USC.StorageConfig -> IO ([Reply])
createIndexes config = mapM (createIndex' config) $ USC.indexes config

createIndex' :: USC.StorageConfig -> USC.StorageIndexSettings -> IO (Reply)
createIndex' config settings = withBH' config $ createIndex (indexSettings settings) (indexName settings)

deleteIndexes :: USC.StorageConfig -> IO ([Reply])
deleteIndexes config = mapM (deleteIndex' config) $ USC.indexes config

deleteIndex' :: USC.StorageConfig -> USC.StorageIndexSettings -> IO (Reply)
deleteIndex' config settings = withBH' config $ deleteIndex (indexName settings)

putMappings :: USC.StorageConfig -> IO ([Reply])
putMappings config = mapM (putMapping' config) $ USC.indexes config

putMapping' :: USC.StorageConfig -> USC.StorageIndexSettings -> IO (Reply)
putMapping' config settings@(USC.StorageIndexSettings key _ _ _)
    | key == "repository" = withBH'' $ putMapping (indexName settings) repositoryMapping RepositoryMapping
    | key == "branch"     = withBH'' $ putMapping (indexName settings) branchMapping BranchMapping
    | key == "commit"     = withBH'' $ putMapping (indexName settings) commitMapping CommitMapping
    | key == "issue"      = withBH'' $ putMapping (indexName settings) issueMapping IssueMapping
    where withBH'' = withBH' config

deleteMappings :: USC.StorageConfig -> IO ([Reply])
deleteMappings config = mapM (deleteMapping' config) $ USC.indexes config

deleteMapping' :: USC.StorageConfig -> USC.StorageIndexSettings -> IO (Reply)
deleteMapping' config settings@(USC.StorageIndexSettings key _ _ _)
    | key == "repository" = withBH'' $ deleteMapping'' repositoryMapping
    | key == "branch"     = withBH'' $ deleteMapping'' branchMapping
    | key == "commit"     = withBH'' $ deleteMapping'' commitMapping
    | key == "issue"      = withBH'' $ deleteMapping'' issueMapping
    where withBH''        = withBH' config
          deleteMapping'' = deleteMapping (indexName settings)

indexDocument' :: (ToJSON doc) => USC.StorageConfig -> USC.StorageIndexSettings -> doc -> IO (Reply)
indexDocument' config settings@(USC.StorageIndexSettings key _ _ _) document
    | key == "repository" = withBH'' $ indexDocument'' repositoryMapping defaultIndexDocumentSettings document
    | key == "branch"     = withBH'' $ indexDocument'' branchMapping defaultIndexDocumentSettings document
    | key == "commit"     = withBH'' $ indexDocument'' commitMapping defaultIndexDocumentSettings document
    | key == "issue"      = withBH'' $ indexDocument'' issueMapping defaultIndexDocumentSettings document
    where withBH''        = withBH' config
          indexDocument'' = indexDocumentAutoID (indexName settings)

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
                .= object [ "name"          .= object [ "type"  .= ("string"       :: T.Text)
                                                      , "index" .= ("not_analyzed" :: T.Text) ]
                          , "type"          .= object [ "type"  .= ("string"       :: T.Text)
                                                      , "index" .= ("not_analyzed" :: T.Text) ]
                          , "url"           .= object [ "type"  .= ("string"       :: T.Text)
                                                      , "index" .= ("not_analyzed" :: T.Text) ]
                          , "previousUrls"  .= object [ "type"  .= ("string"       :: T.Text)
                                                      , "index" .= ("not_analyzed" :: T.Text) ]
                          , "defaultBranch" .= object [ "type"  .= ("nested"       :: T.Text)
                                                      , "properties"
                                                         .= object [ "_id"         .= object [ "type"   .= ("string"       :: T.Text)
                                                                                             , "index"  .= ("not_analyzed" :: T.Text) ]
                                                                   , "name"        .= object [ "type"   .= ("string"       :: T.Text)
                                                                                             , "index"  .= ("not_analyzed" :: T.Text) ]]]
                          , "headCommit"   .= object [ "type"  .= ("nested"       :: T.Text)
                                                     , "properties"
                                                        .= object [ "_id"         .= object [ "type"   .= ("string"       :: T.Text)
                                                                                            , "index"  .= ("not_analyzed" :: T.Text) ]
                                                                  , "hash"        .= object [ "type"   .= ("string"       :: T.Text)
                                                                                            , "index"  .= ("not_analyzed" :: T.Text) ]
                                                                  , "time"        .= object [ "type"   .= ("date"         :: T.Text)
                                                                                            , "format" .= ("epoch_second" :: T.Text) ]
                                                                  , "buildStatus" .= object [ "type"   .= ("string"       :: T.Text)
                                                                                            , "index"  .= ("not_analyzed" :: T.Text) ]]] ]]]


-- Mapping for the "branch" document type.
branchMapping = MappingName "branch"

data BranchMapping = BranchMapping deriving (Eq, Show)

instance ToJSON BranchMapping where
    toJSON BranchMapping
        = object [ "branch"
            .= object [ "properties"
                .= object [ "name"       .= object [ "type"  .= ("string"       :: T.Text)
                                                   , "index" .= ("not_analyzed" :: T.Text) ]
                          , "headCommit" .= object [ "type"  .= ("nested"       :: T.Text)
                                                   , "properties"
                                                       .= object [ "_id"         .= object [ "type"   .= ("string"       :: T.Text)
                                                                                           , "index"  .= ("not_analyzed" :: T.Text) ]
                                                                 , "hash"        .= object [ "type"   .= ("string"       :: T.Text)
                                                                                           , "index"  .= ("not_analyzed" :: T.Text) ]
                                                                 , "time"        .= object [ "type"   .= ("date"         :: T.Text)
                                                                                           , "format" .= ("epoch_second" :: T.Text) ]
                                                                 , "buildStatus" .= object [ "type"   .= ("string"       :: T.Text)
                                                                                           , "index"  .= ("not_analyzed" :: T.Text) ]]] ]]]


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
                                                       .= object [ "hash" .= object [ "type"   .= ("string"       :: T.Text)
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
