{-# LANGUAGE OverloadedStrings #-}

module Unhack.Issue
       ( indexIssue
       , displayIssue
       , accessProperty
       , bulkSetProperty
       , bulkSetCommit
       , Issue(..)
       ) where

{-
  @Issue(
    "Decouple Issue from ElasticSearch",
    type="improvement",
    priority="low",
    labels="modularity"
  )
-}
import Unhack.Commit
import Data.List
import Database.Bloodhound
import Data.Aeson
import qualified Unhack.ElasticSearch as ES

-- Public API.
indexIssue :: (ToJSON doc, MonadBH m) => doc -> m Reply
indexIssue issue = indexDocument ES.index issueMapping defaultIndexDocumentSettings issue (DocId "")

{-
  @Issue(
    "Add spaces so that all property values align with the value of the property
    with the longest key e.g. 'title' value aligns with 'priority' value etc.",
    type="improvement",
    priority="low"
  )
-}
displayIssue :: Issue -> String
displayIssue issue = unlines (["{"] `union` indentedProperties `union` ["}"])
                     where properties = ["file", "title", "kind", "priority", "labels"]
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
accessProperty issue "file" = file issue
accessProperty issue "title" = title issue
accessProperty issue "kind" = kind issue
accessProperty issue "priority" = priority issue
accessProperty issue "labels" = labels issue

bulkSetProperty :: [Issue] -> String -> String -> [Issue]
bulkSetProperty issues propertyKey propertyValue
    | propertyKey == "file" = map (setFile propertyValue) issues
    | propertyKey == "projectId" = map (setProjectId propertyValue) issues

bulkSetCommit :: [Issue] -> Commit -> [Issue]
bulkSetCommit issues commit = map (setCommit commit) issues

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
                   , commit :: Commit
                   , file :: String
                   , title :: String
                   , kind :: String
                   , priority :: String
                   , labels :: String
                   } deriving (Show)

instance ToJSON Issue where
    toJSON (Issue projectId commit file title kind priority labels) = object ["projectId" .= projectId, "commit.hash" .= (hash commit), "commit.time" .= (time commit), "file" .= file, "title" .= title, "type" .= kind, "priority" .= priority, "labels" .= labels]

displayProperty :: Issue -> String -> String
displayProperty issue property = if text == "" then "" else property ++ ": " ++ text
                                 where text = accessProperty issue property

setFile :: String -> Issue -> Issue
setFile fileValue issue = issue { file = fileValue }

setCommit :: Commit -> Issue -> Issue
setCommit commitValue issue = issue { commit = commitValue }

setProjectId :: String -> Issue -> Issue
setProjectId projectIdValue issue = issue { projectId = projectIdValue }
