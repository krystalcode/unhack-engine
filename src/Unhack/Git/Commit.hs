{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Commit
       ( hashesToCommitsText
       , logCommitsText
       , logTextToCommits
       ) where

import qualified Data.Text as T (concat, intercalate, lines, pack, unpack, Text)
import Unhack.Commit
import Unhack.Process


-- Public API.

-- Gets a commit's text in the "git_log" format for the given commit hash.
hashesToCommitsText :: FilePath -> [T.Text] -> IO (T.Text)
hashesToCommitsText directory hashes = lazyProcess command directory
    where command = T.concat ["git show -s --format=%H_%ai ", hashesText]
          hashesText = T.intercalate " " hashes

-- Gets the list of commits as text by executing the "git log" command for the
-- requested branch.
logCommitsText :: FilePath -> T.Text -> Int -> IO (T.Text)
logCommitsText directory branch nbCommits = lazyProcess command directory
    where command = T.concat ["git log ", branch, " --pretty=\"format:%H_%ai\"", nbCommitsOption]
          nbCommitsOption = case nbCommits of 0 -> ""
                                              _ -> T.concat [" -n ", T.pack (show nbCommits)]

-- Takes text containing a list of commits, in the format returned by the
-- "git log" command, and it converts it into a list of Commit records.
logTextToCommits :: T.Text -> [Commit]
logTextToCommits "" = []
logTextToCommits input = map (textToCommit' "git_log") $ T.lines input

-- Functions for internal use.

-- "textToCommit" with its arguments inverted so that it can be passed on to
-- filtering.
textToCommit' :: T.Text -> T.Text -> Commit
textToCommit' format commitText = textToCommit commitText format
