module Unhack.Parser
       ( parseString
       , parseFileString
       , parseCommitFileString
       , parseCommitContents
       ) where


-- Imports.

-- External dependencies.
import Data.List       (intercalate)
import Data.Time       (UTCTime)
import Text.Regex.PCRE ((=~), getAllTextMatches)

import qualified Data.Text as T (unpack, Text)

-- Internal dependencies.

import Unhack.Issue
import Unhack.Data.EmIssueCommit


-- Public API.

{-
  @Issue(
    "Use Text instead of String everywhere in the Parser",
    type="task",
    priority="normal"
  )
-}

parseString :: T.Text -> UTCTime -> [Issue]
parseString input now = map (extractProperties' now) issues
            where issues = extractIssues $ T.unpack input

parseFileString :: T.Text -> T.Text -> UTCTime -> [Issue]
parseFileString file input now = bulkSetProperty issues "file" (T.unpack file)
    where issues = parseString input now

parseCommitFileString :: EmIssueCommit -> T.Text -> T.Text -> UTCTime -> [Issue]
parseCommitFileString commit file input now = bulkSetCommit issues commit
    where issues = parseFileString file input now

-- Given a commit and a list of its files with their contents, return the commit with a list of the issues contained in
-- the given files' contents
parseCommitContents' :: (EmIssueCommit, [(T.Text, T.Text)]) -> UTCTime -> (EmIssueCommit, [Issue])
parseCommitContents' (commit, contents) now = (commit, concat $ map (\(file, content) -> parseCommitFileString commit file content now) contents)

parseCommitContents :: UTCTime -> (EmIssueCommit, [(T.Text, T.Text)]) -> (EmIssueCommit, [Issue])
parseCommitContents now contents = parseCommitContents' contents now


-- Functions for internal use.
extractIssues :: String -> [String]
extractIssues input = trimIssues . regexAllMatches $ input

regexAllMatches :: String -> [String]
regexAllMatches input = getAllTextMatches $ input =~ "@Issue\\(([\\s\\S]+?)\\)" :: [String]

trimIssues :: [String] -> [String]
trimIssues xs = map (takeWhile (/=')') . tail . dropWhile (/='(')) xs

extractProperties :: String -> UTCTime -> Issue
extractProperties issue now
    = makeIssue (propertyList !! 0) (propertyList !! 1) (propertyList !! 2) (propertyStringToList $ propertyList !! 3) now

    where title = extractTitle issue
          optionalPropertyKeys = ["type", "priority", "labels"]
          optionalProperties = map (extractProperty issue) optionalPropertyKeys
          propertyList = [title] ++ optionalProperties

extractProperties' :: UTCTime -> String -> Issue
extractProperties' now issue = extractProperties issue now

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
