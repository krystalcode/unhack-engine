module Unhack.Parser
    ( parseCommitContents
    ) where


-- Imports.

-- External dependencies.
import Data.List       (dropWhile, intercalate)
import Data.Maybe      (fromJust, isNothing)
import Data.Time       (UTCTime)
import Text.Regex.PCRE ((=~), getAllTextMatches)

import qualified Data.Text as T (null, pack, unpack, Text)

-- Internal dependencies.

import Unhack.Data.EmIssueCommit   (EmIssueCommit)
import Unhack.Data.IssueProperties (IssueProperties(..))
import Unhack.Util                 (csvToList)


-- Public API.

{-
  @Issue(
    "Use Text instead of String everywhere in the Parser",
    type="improvement",
    priority="low"
    labels="performance"
  )
-}

-- Given a commit and a list of its files with their contents, return the commit with a list of the issues contained in
-- the given files' contents
parseCommitContents :: (EmIssueCommit, [(T.Text, T.Text)]) -> (EmIssueCommit, [(T.Text, [IssueProperties])])
parseCommitContents (commit, contents) = (commit, map (\(file, content) -> parseFileString file content) contents)


-- Functions for internal use.

parseString :: T.Text -> [IssueProperties]
parseString input = map extractProperties issues
            where issues = extractIssues $ T.unpack input

parseFileString :: T.Text -> T.Text -> (T.Text, [IssueProperties])
parseFileString file input = (file, parseString input)

extractIssues :: String -> [String]
extractIssues input = trimIssues . regexAllMatches $ input

regexAllMatches :: String -> [String]
regexAllMatches input = getAllTextMatches $ input =~ "@Issue\\(([\\s\\S]+?)\\)" :: [String]

trimIssues :: [String] -> [String]
trimIssues xs = map (takeWhile (/=')') . tail . dropWhile (/='(')) xs

extractProperties :: String -> IssueProperties
extractProperties issue
    = IssueProperties
        { labels   = extractMultiValueProperty issue "labels"
        , priority = extractProperty issue "priority"
        , title    = extractTitle issue
        , type'    = extractProperty issue "type"
        }

extractMultiValueProperty :: String -> String -> Maybe [T.Text]
extractMultiValueProperty issue property
    = if   isNothing maybeCSVValue
      then Nothing
      else Just (csvToList $ fromJust maybeCSVValue)

    where maybeCSVValue = extractProperty issue property

extractProperty :: String -> String -> Maybe T.Text
extractProperty issue property = if value == "" then Nothing else Just (T.pack value)
                where pattern = property ++ "=\"([^\"]+)\""
                      value   = stripNewLines . trimProperty $ (issue =~ pattern :: String)

extractTitle :: String -> T.Text
extractTitle issue = if T.null validTitle then T.pack issue else validTitle
                     where maybeTitleWithKey = extractProperty issue "title"
                           {-
                             @Issue(
                               "Reuse extractProperty by passing pattern as an argument",
                               type="improvement",
                               priority="low",
                               labels="refactoring"
                             )
                           -}
                           titleWithoutKey = stripNewLines . trimProperty $ (issue =~ "\"([^\"]+)\"" :: String)
                           validTitle = if isNothing maybeTitleWithKey then T.pack titleWithoutKey else fromJust maybeTitleWithKey

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
