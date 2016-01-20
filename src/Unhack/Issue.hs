{-# LANGUAGE OverloadedStrings #-}

module Unhack.Issue
       ( accessProperty
       , bulkSetProperty
       , bulkSetCommit
       , displayIssue
       , emptyIssue
       , Issue(..)
       ) where

import Unhack.Commit
import Data.List
import Data.Aeson

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

bulkSetCommit :: [Issue] -> Commit -> [Issue]
bulkSetCommit issues commit = map (setCommit commit) issues

-- Functions for internal use.

data Issue = Issue { commit :: Commit
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

emptyIssue = Issue { commit   = emptyCommit
                   , file     = ""
                   , title    = ""
                   , kind     = ""
                   , priority = ""
                   , labels   = "" }

instance ToJSON Issue where
    toJSON (Issue commit file title kind priority labels) =
        object [ "commit.hash" .= (hash commit)
               , "commit.time" .= (time commit)
               , "file" .= file
               , "title" .= title
               , "type" .= kind
               , "priority" .= priority
               , "labels" .= labels ]

displayProperty :: Issue -> String -> String
displayProperty issue property = if text == "" then "" else property ++ ": " ++ text
                                 where text = accessProperty issue property

setFile :: String -> Issue -> Issue
setFile fileValue issue = issue { file = fileValue }

setCommit :: Commit -> Issue -> Issue
setCommit commitValue issue = issue { commit = commitValue }
