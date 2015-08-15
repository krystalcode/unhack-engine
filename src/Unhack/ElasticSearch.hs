{-# LANGUAGE OverloadedStrings #-}

module Unhack.ElasticSearch
       ( withBH'
       -- The following should be configuration.
       , server
       , index
       , indexSettings
       ) where

import Database.Bloodhound
import Network.HTTP.Client

withBH' = withBH defaultManagerSettings server

createIndex = withBH' $ createIndex indexSettings index

server = Server "http://elk:9200"
index = IndexName "unhack"
indexSettings = IndexSettings (ShardCount 1) (ReplicaCount 0)
