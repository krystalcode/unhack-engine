module Unhack.Parser
       ( parseString
       , parseFileString
       , parseFileString'
       , parseCommitFileString
       , parseCommitFileString'
       ) where

import qualified Data.Text as T (unpack, Text)
import Data.List (intercalate)
import Text.Regex.PCRE ((=~), getAllTextMatches)
import Unhack.Issue
import Unhack.Data.EmIssueCommit

{-
  @Issue(
    "Use Text instead of String everywhere in the Parser",
    type="task",
    priority="normal"
  )
-}

-- Public API.
parseString :: T.Text -> [Issue]
parseString input = map extractProperties issues
            where issues = extractIssues $ T.unpack input

parseFileString :: T.Text -> T.Text -> [Issue]
parseFileString file input = bulkSetProperty issues "file" (T.unpack file)
    where issues = parseString input

parseFileString' :: (T.Text, T.Text) -> [Issue]
parseFileString' (file, input) = parseFileString file input

parseCommitFileString :: EmIssueCommit -> T.Text -> T.Text -> [Issue]
parseCommitFileString commit file input = bulkSetCommit issues commit
    where issues = parseFileString file input

parseCommitFileString' :: (EmIssueCommit, T.Text, T.Text) -> [Issue]
parseCommitFileString' (commit, file, input) = parseCommitFileString commit file input

-- Functions for internal use.
extractIssues :: String -> [String]
extractIssues input = trimIssues . regexAllMatches $ input

regexAllMatches :: String -> [String]
regexAllMatches input = getAllTextMatches $ input =~ "@Issue\\(([\\s\\S]+?)\\)" :: [String]

trimIssues :: [String] -> [String]
trimIssues xs = map (takeWhile (/=')') . tail . dropWhile (/='(')) xs

extractProperties :: String -> Issue
extractProperties issue =
    emptyIssue { title = propertyList !! 0
               , kind = propertyList !! 1
               , priority = propertyList !! 2
               , labels = propertyList !! 3 }
    where title = extractTitle issue
          optionalPropertyKeys = ["type", "priority", "labels"]
          optionalProperties = map (extractProperty issue) optionalPropertyKeys
          propertyList = [title] ++ optionalProperties

{-
  @Issue(
    "Allow for different regexps per property e.g. do not allow special
    characters in type, priority and labels etc.",
    type="bug",
    priority="normal"
  )
-}
extractProperty :: String -> String -> String
extractProperty issue property = stripNewLines . trimProperty $ (issue =~ pattern :: String)
                where pattern = property ++ "=\"([^\"]+)\""

extractTitle :: String -> String
extractTitle issue = if validTitle /= "" then validTitle else issue
                     where titleWithKey = extractProperty issue "title"
                           {-
                             @Issue(
                               "Reuse extractProperty when it accepts a pattern as an argument",
                               type="improvement",
                               priority="low",
                               labels="refactoring"
                             )
                           -}
                           titleWithoutKey = stripNewLines . trimProperty $ (issue =~ "\"([^\"]+)\"" :: String)
                           validTitle = if titleWithKey /= "" then titleWithKey else titleWithoutKey

stripNewLines :: String -> String
stripNewLines [] = []
stripNewLines property = intercalate " " . map (trimLineBeginning) $ splitLines
              where splitLines = lines property

trimLineBeginning :: String -> String
trimLineBeginning [] = []
trimLineBeginning line = dropWhile(`elem` " /*") line

trimProperty :: String -> String
trimProperty [] = []
trimProperty property = takeWhile (/='"') . tail . dropWhile (/='"') $ property
