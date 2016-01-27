{-# LANGUAGE OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Operations
       ( createIndex'
       , deleteIndex'
       , putMapping'
       , deleteMapping'
       , indexIssue
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

createIndex' :: USC.StorageConfig -> IO (Reply)
createIndex' config = withBH' config $ createIndex (indexSettings config) (indexName config)

deleteIndex' :: USC.StorageConfig -> IO (Reply)
deleteIndex' config = withBH' config $ deleteIndex (indexName config)

putMapping' :: USC.StorageConfig -> IO (Reply)
putMapping' config = withBH' config $ putMapping (indexName config) issueMapping IssueMapping

deleteMapping' :: USC.StorageConfig -> IO (Reply)
deleteMapping' config = withBH' config $ deleteMapping (indexName config) issueMapping

indexIssue :: (ToJSON doc) => USC.StorageConfig -> doc -> IO (Reply)
indexIssue config issue = withBH' config $ indexDocumentAutoID index issueMapping defaultIndexDocumentSettings issue
    where index = indexName config

bulkIndexIssues :: USC.StorageConfig -> [Issue] -> IO (Reply)
bulkIndexIssues config issues = withBH' config $ bulk (fromList ops)
    where ops = [BulkIndex index issueMapping (DocId "") (toJSON issue) | issue <- issues]
          index = indexName config


-- Functions/types for internal use.

withBH' :: USC.StorageConfig -> BH IO a -> IO a
withBH' config = withBH defaultManagerSettings $ server config

server :: USC.StorageConfig -> Server
server config = Server $ T.concat [host, ":", T.pack(show port)]
    where host = USC.host config
          port = USC.port config

indexName :: USC.StorageConfig -> IndexName
indexName config = IndexName $ USC.name (USC.index config)

indexSettings :: USC.StorageConfig -> IndexSettings
indexSettings config = IndexSettings (ShardCount shardCount) (ReplicaCount replicaCount)
    where shardCount = USC.shards (USC.index config)
          replicaCount = USC.replicas (USC.index config)

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
                .= object [ "repository"
                              .= object [ "type" .= ("nested" :: T.Text)
                                        , "properties"
                                            .= object [ "id" .= object [ "type" .= ("string" :: T.Text)
                                                                       , "index" .= ("not_analyzed" :: T.Text) ],
                                                        "url" .= object [ "type" .= ("string" :: T.Text)
                                                                        , "index" .= ("not_analyzed" :: T.Text) ]]]
                          , "commit"
                              .= object [ "type" .= ("nested" :: T.Text)
                                        , "properties"
                                            .= object [ "hash" .= object [ "type" .= ("string" :: T.Text)
                                                                         , "index" .= ("not_analyzed" :: T.Text) ],
                                                        "time" .= object [ "type" .= ("date" :: T.Text)
                                                                         , "format" .= ("epoch_second" :: T.Text) ]]]
                          , "file"
                              .= object [ "type" .= ("string" :: T.Text)
                                        , "index" .= ("not_analyzed" :: T.Text) ]
                          , "title"
                              .= object [ "type" .= ("string" :: T.Text)
                                        , "index" .= ("analyzed" :: T.Text) ]
                          , "type"
                              .= object [ "type" .= ("string" :: T.Text)
                                        , "index" .= ("not_analyzed" :: T.Text) ]
                          , "priority"
                              .= object [ "type" .= ("string" :: T.Text)
                                        , "index" .= ("not_analyzed" :: T.Text) ]
                          , "labels"
                              .= object [ "type" .= ("string" :: T.Text)
                                        , "index" .= ("not_analyzed" :: T.Text) ] ]]]
