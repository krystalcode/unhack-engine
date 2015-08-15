module Unhack.Issue
       ( Issue(..)
       ) where

import Data.Aeson
import qualified Unhack.ElasticSearch as ES

issueMapping = MappingName "issue"
indexIssue = withBH' $ indexDocument ES.index issueMapping defaultIndexDocumentSettings

data Issue = Issue { projectId :: String
                   , commit :: String
                   , title :: String
                   , kind :: String
                   , priority :: String
                   , labels :: String
                   } deriving (Show)

instance ToJSON Issue where
    toJSON (Issue title kind priority labels) = object ["title" .= title, "type" .= kind, "priority" .= priority, "labels" .= labels]
