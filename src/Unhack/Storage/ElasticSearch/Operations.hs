{-# LANGUAGE OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Operations
       ( createIndex'
       , indexIssue
       , bulkIndexIssues
       ) where

import qualified Unhack.Storage.ElasticSearch.Config as USC
import Unhack.Issue
import Unhack.Commit
import Data.Vector (fromList)
import Data.Aeson
import qualified Data.Text as T (concat, pack)
import Database.Bloodhound
import Network.HTTP.Client (defaultManagerSettings)

{-
    @Issue(
        "Review the changes for automatic ID generation and submit a pull request"
        type="task"
        priority="normal"
        labels="contributions"
    )
-}

-- Public API.

createIndex' :: USC.StorageConfig -> IO Reply
createIndex' config = withBH' config $ createIndex (indexSettings config) (indexName config)

indexIssue :: (ToJSON doc) => USC.StorageConfig -> doc -> IO Reply
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

{-
    @Issue(
        "Properly define the 'issue' document mapping"
        type="bug"
        priority="normal"
    )
-}
issueMapping = MappingName "issue"
