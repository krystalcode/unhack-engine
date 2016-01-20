{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Tree
       ( commitTree
       , commitTree'
       , treeValidFiles
       , treeValidFiles'
       ) where

import qualified Data.Text as T (concat, filter, isPrefixOf, isSuffixOf, lines, null, pack, unpack, Text)
import Unhack.Commit
import Unhack.Process


-- Public API.

-- Get the full git tree on a specific commit.
commitTree :: FilePath -> Commit -> IO (T.Text)
commitTree directory commit = strictProcess command directory
    where command = T.concat ["git ls-tree --full-tree --name-only -r ", (hash commit)]

commitTree' :: FilePath -> Commit -> IO (Commit, T.Text)
commitTree' directory commit = do
    tree <- commitTree directory commit
    return (commit, tree)

-- Convert a git tree (that contains all file paths as one text string) to a
-- list of file paths, excluding file paths that are not valid under the
-- requested schema.
treeValidFiles :: [T.Text] -> [T.Text] -> [T.Text] -> T.Text -> [T.Text]
treeValidFiles validPaths invalidPaths validExtensions tree = validFiles
    where files = filter (not . T.null) $ T.lines tree
          validFiles = filter (\x -> hasValidPath x validPaths && not (hasInvalidPath x invalidPaths) && hasValidExtension x validExtensions) files

treeValidFiles' :: [T.Text] -> [T.Text] -> [T.Text] -> (Commit, T.Text) -> [(Commit, T.Text)]
treeValidFiles' validPaths invalidPaths validExtensions (commit, tree) = zipWithCommit commit validFiles
    where validFiles = treeValidFiles validPaths invalidPaths validExtensions tree
          zipWithCommit commitRecord filesList = [(commitRecord, file) | file <- filesList]


-- Functions for internal use.

-- Functions for determining whether a given file is valid under the requested
-- schema.

{-
    @Issue(
        "Is there any benefit in using FilePath instead of Text?"
        type="investigation"
        priority="low"
    )
-}

hasPath :: T.Text -> T.Text -> Bool
hasPath file folder = folder `T.isPrefixOf` file

hasValidPath :: T.Text -> [T.Text] -> Bool
hasValidPath _ [] = True
hasValidPath file validPaths = elem True $ map (hasPath file) validPaths

hasInvalidPath :: T.Text -> [T.Text] -> Bool
hasInvalidPath _ [] = False
hasInvalidPath file invalidPaths = elem True $ map (hasPath file) invalidPaths

hasExtension :: T.Text -> T.Text -> Bool
hasExtension file extension = (T.concat [".", extension]) `T.isSuffixOf` file

{-
    @Issue(
        "Allow empty list as an argument to valid extensions after we can
        detect binary files"
        type="bug"
        priority="normal"
    )
-}

hasValidExtension :: T.Text -> [T.Text] -> Bool
hasValidExtension file [] = error . T.unpack $ T.concat
    [ "A list of valid extensions must be specified in order to validate the extension of the file \""
    , file
    , "\"" ]
hasValidExtension file validExtensions = elem True $ map (hasExtension file) validExtensions
