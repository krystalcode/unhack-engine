{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Contents
       ( fileContents
       , fileContents'
       ) where

import qualified Data.Text as T (concat, unpack, Text)
import System.Exit (ExitCode(ExitFailure))
import Unhack.Data.EmIssueCommit
import Unhack.Process


-- Public API.

-- Get the contents of a file on a specific commit.
fileContents :: FilePath -> EmIssueCommit -> T.Text -> IO (T.Text)
fileContents directory commit file = do
    contents <- strictProcess command directory
    return $ either handleLeft id contents

    where command = T.concat ["git show ", (hash commit), ":", file]

          -- If the file does not exist (128) or an unknown exception has
          -- occurred (0, most likely encoding error), return an empty string as
          -- the contents of the file. Otherwise throw an error with the
          -- failure's exit code.
          handleLeft exitCode
              | exitCode == ExitFailure 0   = ""
              | exitCode == ExitFailure 128 = ""
              | otherwise                   = error $ show exitCode

fileContents' :: FilePath -> (EmIssueCommit, T.Text) -> IO (EmIssueCommit, T.Text, T.Text)
fileContents' directory (commit, file) = do
    contents <- fileContents directory commit file
    return (commit, file, contents)
