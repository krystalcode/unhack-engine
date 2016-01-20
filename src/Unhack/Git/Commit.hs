{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Commit
       ( logCommitsText
       , logTextToCommits
       ) where

import qualified Data.Text as T (lines, unpack, Text)
import Unhack.Commit
import Unhack.Process


-- Public API.

-- Gets the list of commits as text by executing the "git log" command for the
-- requested branch.
logCommitsText :: FilePath -> T.Text -> Int -> IO (String)
logCommitsText directory branch nbCommits = lazyProcess command directory
    where command = "git log " ++ (T.unpack branch) ++ " --pretty=\"format:%H_%ai\"" ++ nbCommitsOption
          nbCommitsOption = case nbCommits of 0 -> ""
                                              _ -> " -n " ++ (show nbCommits)

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
