{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Contents
       ( fileContents
       , fileContents'
       ) where

import qualified Data.Text as T (unpack, Text)
import Unhack.Commit
import Unhack.Process


-- Public API.

-- Get the contents of a file on a specific commit.
fileContents :: FilePath -> Commit -> T.Text -> IO (String)
fileContents directory commit file = strictProcess command directory
    where command = "git show " ++ (T.unpack $ hash commit) ++ ":" ++ (T.unpack file)

fileContents' :: FilePath -> Commit -> T.Text -> IO (Commit, String)
fileContents' directory commit file = do
    contents <- fileContents directory commit file
    return (commit, contents)
