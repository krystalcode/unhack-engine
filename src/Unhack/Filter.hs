module Unhack.Filter
       ( filterIssues
       ) where

import Unhack.Issue
import Data.List.Split

-- Public API.
{-
  @Issue(
    "Check if there is a way to do this without double filtering",
    type="improvement",
    priority="low",
    labels="performance"
  )
-}
filterIssues :: [Issue] -> String -> [Issue]
filterIssues issues [] = issues
filterIssues [] _ = []
filterIssues issues filterValue = filter (applyFilters filters) issues
                                  where filters = splitOn "&" filterValue

-- Functions for internal use.
{-
  @Issue(
    "Check if something like 'or $ map comparison filters' is more efficient",
    type="improvement",
    priority="low",
    labels="performance"
  )
-}
applyFilters :: [String] -> Issue -> Bool
applyFilters filters issue = [] /= filter (applyFilter issue) filters

{-
  @Issue(
    "Support comma-separated values for the 'labels' property",
    type="improvement",
    priority="normal"
  )
-}
applyFilter :: Issue -> String -> Bool
applyFilter issue filterValue = accessProperty issue property == value
                                where [property, value] = splitOn "=" filterValue
