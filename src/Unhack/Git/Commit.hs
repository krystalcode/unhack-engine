{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Commit
       ( getCommits
       , hashesToCommitsText
       , logCommitsText
       , logTextToCommits
       ) where


-- Imports.

-- External dependencies.

import qualified Data.Text as T (concat, intercalate, lines, pack, unpack, Text)

-- Internal dependencies.

import Unhack.Data.EmIssueCommit
import Unhack.Process


-- Public API.

getCommits :: FilePath -> [T.Text] -> T.Text -> IO ([EmIssueCommit])
getCommits directory commits branch = getCommits' directory branch commits

-- Gets a commit's text in the "hash_unix_timestamp" format for the given commit
-- hash.
hashesToCommitsText :: FilePath -> [T.Text] -> IO (T.Text)
hashesToCommitsText directory hashes = lazyProcess command directory
    where command = T.concat ["git show -s --format=%H_%at ", hashesText]
          hashesText = T.intercalate " " hashes

-- Gets the list of commits as text by executing the "git log" command for the
-- requested branch.
logCommitsText :: FilePath -> T.Text -> Int -> IO (T.Text)
logCommitsText directory branch nbCommits = lazyProcess command directory
    where command = T.concat ["git log ", branch, " --pretty=\"format:%H_%at\"", nbCommitsOption]
          nbCommitsOption = case nbCommits of 0 -> ""
                                              _ -> T.concat [" -n ", T.pack (show nbCommits)]

-- Takes text containing a list of commits, in the format returned by the
-- "git log" command, and it converts it into a list of Commit records.
logTextToCommits :: T.Text -> [EmIssueCommit]
logTextToCommits "" = []
logTextToCommits input = map (textToCommit' "hash_unix_timestamp") $ T.lines input

-- Functions for internal use.

-- Get the EmIssueCommit object(s) as a list for:
-- - All the commits in the given branch, if "all" is given as the only commit.
-- - The given commits, if the list of commit does not contain only "all".
-- - Or, the latest commit on the given branch, if no commit was given.
-- - Or, the HEAD, if no branch was given.
getCommits' :: FilePath -> T.Text -> [T.Text] -> IO ([EmIssueCommit])
getCommits' directory branch ["all"] = do
    commitsText <- logCommitsText directory branch 0
    return $ logTextToCommits commitsText

getCommits' directory "" [] = do
    commitsText <- hashesToCommitsText directory ["HEAD"]
    return $ logTextToCommits commitsText

getCommits' directory branch [] = do
    commitsText <- logCommitsText directory branch 1
    return $ logTextToCommits commitsText

getCommits' directory _ commits = do
    commitsText <- hashesToCommitsText directory commits
    return $ logTextToCommits commitsText

-- "textToCommit" with its arguments inverted so that it can be passed on to
-- filtering.
textToCommit' :: T.Text -> T.Text -> EmIssueCommit
textToCommit' format commitText = textToEmIssueCommit commitText format
