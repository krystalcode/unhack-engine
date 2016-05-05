{-# LANGUAGE OverloadedStrings #-}

module Unhack.Issue
       ( accessProperty
       , bulkSetProperty
       , bulkSetRepository
       , bulkSetCommit
       , displayIssue
       , emptyIssue
       , propertyStringToList
       , Issue(..)
       ) where

import Data.Char (isSpace)
import Data.List
import qualified Data.Text as T (dropWhile, dropWhileEnd, null, pack, splitOn, unpack)
import Data.Aeson
import Unhack.Data.EmIssueCommit
import Unhack.Data.EmbeddedRepository

{-
  @Issue(
    "Use Text instead of String everywhere in the Issue module",
    type="task",
    priority="low"
  )
-}

-- Public API.

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

{-
  @Issue(
    "There must be a better way than pattern matching",
    type="bug",
    priority="normal",
  )
  @Issue(
    "Replace all calls for the 'kind' property with calls for the 'type' property",
    type="refactoring",
    priority="normal",
  )
-}
accessProperty :: Issue -> String -> String
accessProperty issue "file" = file issue
accessProperty issue "title" = title issue
accessProperty issue "type" = kind issue
accessProperty issue "kind" = kind issue
accessProperty issue "priority" = priority issue
accessProperty issue "labels" = labels issue

-- Convert a multi-value property from a comma-separated string to a list of strings.
propertyStringToList :: String -> [String]
propertyStringToList labels = map T.unpack $ filter (not . T.null) trimmed
    where list    = T.splitOn "," $ T.pack labels
          trimmed = map (T.dropWhile isSpace . T.dropWhileEnd isSpace) list

bulkSetProperty :: [Issue] -> String -> String -> [Issue]
bulkSetProperty issues propertyKey propertyValue
    | propertyKey == "file" = map (setFile propertyValue) issues

bulkSetCommit :: [Issue] -> EmIssueCommit -> [Issue]
bulkSetCommit issues commit = map (setCommit commit) issues

bulkSetRepository :: [Issue] -> EmbeddedRepository -> [Issue]
bulkSetRepository issues repository = map (setRepository repository) issues

-- Functions for internal use.

data Issue = Issue { repository :: EmbeddedRepository
                   , commit :: EmIssueCommit
                   {-
                     @Issue(
                       "Support path hierarchy in file property",
                       type="improvement",
                       priority="high"
                     )
                   -}
                   , file :: String
                   , title :: String
                   , kind :: String
                   , priority :: String
                   {-
                     @Issue(
                       "Labels should be an array of strings"
                       type="bug"
                       priority="high"
                     )
                   -}
                   , labels :: String
                   } deriving (Show)

emptyIssue = Issue { repository = emptyEmbeddedRepository
                   , commit     = emptyEmIssueCommit
                   , file       = ""
                   , title      = ""
                   , kind       = ""
                   , priority   = ""
                   , labels     = "" }

instance ToJSON Issue where
    toJSON (Issue repository commit file title kind priority labels) =
        object [ "repository" .= repository
               , "commit" .= commit
               , "file" .= file
               , "title" .= title
               , "type" .= kind
               , "priority" .= priority
               , "labels" .= labels ]

displayProperty :: Issue -> String -> String
displayProperty issue property = if text == "" then "" else property ++ ": " ++ text
                                 where text = accessProperty issue property

setRepository :: EmbeddedRepository -> Issue -> Issue
setRepository repositoryValue issue = issue { repository = repositoryValue }

setFile :: String -> Issue -> Issue
setFile fileValue issue = issue { file = fileValue }

setCommit :: EmIssueCommit -> Issue -> Issue
setCommit commitValue issue = issue { commit = commitValue }
