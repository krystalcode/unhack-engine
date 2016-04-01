{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Contents
       ( fileContents
       , fileContents'
       ) where

import qualified Data.Text as T (concat, unpack, Text)
import Unhack.Data.EmIssueCommit
import Unhack.Process


-- Public API.

-- Get the contents of a file on a specific commit.
fileContents :: FilePath -> EmIssueCommit -> T.Text -> IO (T.Text)
fileContents directory commit file = strictProcess command directory
    where command = T.concat ["git show ", (hash commit), ":", file]

fileContents' :: FilePath -> (EmIssueCommit, T.Text) -> IO (EmIssueCommit, T.Text, T.Text)
fileContents' directory (commit, file) = do
    contents <- fileContents directory commit file
    return (commit, file, contents)
