{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Commit
       ( getHeads
       , list
       ) where


-- Imports.

-- External dependencies.

import Data.Time (UTCTime)

import qualified Data.Map  as M (fromListWith, Map)
import qualified Data.Text as T (concat, filter, intercalate, lines, pack, splitOn, unpack, Text)

-- Internal dependencies.

import Unhack.Data.EmIssueCommit
import Unhack.Process
import Unhack.Util               (posixToUTC)

import qualified Unhack.Data.GitCommit as UDGC


-- Public API.

-- Get a Map of commits for the given branches. The Map item for each commit contains the commit's 'hash' as the key,
-- and a tuple with the commit's time and the list of branches it belongs to as the value (the list of branches only
-- from the given branches).
list :: FilePath -> [T.Text] -> IO (M.Map T.Text (UTCTime, [T.Text]))
list directory branches = do
    texts <- mapM (logCommitsText' directory 0) branches

    let commitTuples = map ((textsToTuples "hash_unix_timestamp") . T.lines) texts
    let commitsWithBranches = concat $ zipWith (\branch commits -> foldr (\(hash, time) acc -> (hash, (time, [branch])):acc) [] commits) branches commitTuples

    return $ M.fromListWith (\(time1, branches1) (time2, branches2) -> (time1, branches1 ++ branches2)) commitsWithBranches

-- Get the head commits of the given branches.
getHeads :: FilePath -> [T.Text] -> IO ([Maybe UDGC.GitCommit])
getHeads directory branches = mapM (getHead directory) branches

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
logTextToCommits :: T.Text -> [UDGC.GitCommit]
logTextToCommits "" = []
logTextToCommits input = map UDGC.fromText $ T.lines input

-- Functions for internal use.

-- Get the head of the given branch.
getHead :: FilePath -> T.Text -> IO (Maybe UDGC.GitCommit)
getHead directory branch = do
    commits <- getCommits' directory branch []
    let commit = case length commits of
                     0 -> Nothing
                     _ -> Just (commits !! 0)
    return commit

-- Get the GitCommit object(s) as a list for:
-- - All the commits in the given branch, if "all" is given as the only commit.
-- - The given commits, if the list of commit does not contain only "all".
-- - Or, the latest commit on the given branch, if no commit was given.
-- - Or, the HEAD, if no branch was given.
getCommits' :: FilePath -> T.Text -> [T.Text] -> IO ([UDGC.GitCommit])
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

-- Converts a git commit text, as provided by 'git log', to a tuple of values.
-- The text needs to be in one of the following supported formats:
-- - "hash_unix_timestamp": The commit's hash and time in unix timestamp format,
--   separated by underscore. For example:
--   b298cff142193c444af4bf4c4ec68f618396c059_1453917211
textToTuples :: T.Text -> T.Text -> (T.Text, UTCTime)

-- 'hash_unix_timestamp'.
textToTuples "hash_unix_timestamp" text = (list !! 0, posixToUTC $ list !! 1)
    where list = T.splitOn "_" text

textToTuples commitText format = error . T.unpack $ T.concat ["The requested text format \"", format, "\" is not supported for converting text to a Commit record."]

-- Convert a list of commits texts to tuples.
textsToTuples :: T.Text -> [T.Text] -> [(T.Text, UTCTime)]
textsToTuples format texts = map (textToTuples format) texts
