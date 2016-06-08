{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Commit
       ( hashesToCommitsText
       , list
       , logCommitsText
       , logTextToCommits
       ) where


-- Imports.

-- External dependencies.

import qualified Data.Text as T (concat, intercalate, lines, pack, unpack, Text)

-- Internal dependencies.

import qualified Data.Map as M (fromListWith, Map)

import Unhack.Data.EmIssueCommit
import Unhack.Process


-- Public API.

-- Get a Map of commits for the given branches. The Map item for each commit contains the commit's 'hash' as the key,
-- and a tuple with the commit's time and the list of branches it belongs to as the value (the list of branches only
-- from the given branches).
list :: FilePath -> [T.Text] -> IO (M.Map T.Text (T.Text, [T.Text]))
list directory branches = do
    texts <- mapM (logCommitsText' directory 0) branches

    let commitTuples = map ((textsToTuples "hash_unix_timestamp") . T.lines) texts
    let commitsWithBranches = concat $ zipWith (\branch commits -> foldr (\(hash, time) acc -> (hash, (time, [branch])):acc) [] commits) branches commitTuples

    return $ M.fromListWith (\(time1, branches1) (time2, branches2) -> (time1, branches1 ++ branches2)) commitsWithBranches

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

logCommitsText' :: FilePath -> Int -> T.Text -> IO (T.Text)
logCommitsText' directory nbCommits branch = logCommitsText directory branch nbCommits

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
