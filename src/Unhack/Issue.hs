{-# LANGUAGE OverloadedStrings #-}

module Unhack.Issue
       ( indexIssue
       , displayIssue
       , accessProperty
       , Issue(..)
       ) where

import Data.List
import Database.Bloodhound
import Data.Aeson
import qualified Unhack.ElasticSearch as ES

-- Public API.
indexIssue :: (ToJSON doc, MonadBH m) => doc -> m Reply
indexIssue issue = indexDocument ES.index issueMapping defaultIndexDocumentSettings issue (DocId "")

displayIssue :: Issue -> String
displayIssue issue = unlines (["{"] `union` indentedProperties `union` ["}"])
                     where properties = ["title", "kind", "priority", "labels"]
                           renderedProperties = map (displayProperty issue) properties
                           nonEmptyProperties = filter (\x -> x /= "") renderedProperties
                           indentedProperties = map ("    " ++) nonEmptyProperties

{--
  @Issue(
    "There must be a better way than pattern matching",
    type="bug",
    priority="normal",
  )
-}
accessProperty :: Issue -> String -> String
accessProperty issue "title" = title issue
accessProperty issue "kind" = kind issue
accessProperty issue "priority" = priority issue
accessProperty issue "labels" = labels issue

-- Functions for internal use.
issueMapping = MappingName "issue"

{-
  @Issue(
    "Add file property",
    type="improvement",
    priority="high"
  )
-}
data Issue = Issue { projectId :: String
                   , commit :: String
                   , title :: String
                   , kind :: String
                   , priority :: String
                   , labels :: String
                   } deriving (Show)

instance ToJSON Issue where
    toJSON (Issue projectId commit title kind priority labels) = object ["projectId" .= projectId, "commit" .= commit, "title" .= title, "type" .= kind, "priority" .= priority, "labels" .= labels]

displayProperty :: Issue -> String -> String
displayProperty issue property = if text == "" then "" else property ++ ": " ++ text
                                 where text = accessProperty issue property
