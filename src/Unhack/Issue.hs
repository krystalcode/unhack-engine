{-# LANGUAGE OverloadedStrings #-}

module Unhack.Issue
       ( indexIssue
       , Issue(..)
       ) where

import Data.List
import Database.Bloodhound
import Data.Aeson
import qualified Unhack.ElasticSearch as ES

-- Public API.
indexIssue :: (ToJSON doc, MonadBH m) => doc -> m Reply
indexIssue issue = indexDocument ES.index issueMapping defaultIndexDocumentSettings issue (DocId "")

-- Functions for internal use.
issueMapping = MappingName "issue"

data Issue = Issue { projectId :: String
                   , commit :: String
                   , title :: String
                   , kind :: String
                   , priority :: String
                   , labels :: String
                   } deriving (Show)

instance ToJSON Issue where
    toJSON (Issue projectId commit title kind priority labels) = object ["projectId" .= projectId, "commit" .= commit, "title" .= title, "type" .= kind, "priority" .= priority, "labels" .= labels]
