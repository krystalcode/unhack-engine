{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Tree
       ( commitTree
       , commitTree'
       , commitTreeLength
       , treeGlobFilter
       , treeGlobFilter'
       ) where

import qualified Data.List as L (intersect)
import qualified Data.Text as T (concat, filter, isPrefixOf, isSuffixOf, lines, null, pack, unpack, Text)
import System.FilePath.Glob (compile, match, Pattern)
import qualified Unhack.Config as UC (Config(..), Annotation(..), Analysis(..), FilePatterns(..))
import Unhack.Data.EmIssueCommit
import Unhack.Process


-- Public API.

-- Get the full git tree on a specific commit.
commitTree :: FilePath -> EmIssueCommit -> IO (T.Text)
commitTree directory commit = do
    tree <- strictProcess command directory
    return $ either handleLeft id tree

    where command = T.concat ["git ls-tree --full-tree --name-only -r ", (hash commit)]
          handleLeft exitCode = error $ show exitCode

commitTree' :: FilePath -> EmIssueCommit -> IO (EmIssueCommit, [T.Text])
commitTree' directory commit = do
    tree <- commitTree directory commit
    return (commit, filter (not . T.null) $ T.lines tree)

-- Get the number of files on a specific commit.
commitTreeLength :: FilePath -> EmIssueCommit -> IO (Int)
commitTreeLength directory commit = do
    tree <- strictProcess command directory
    return $ either handleLeft treeLength tree

    where command = T.concat ["git ls-tree --full-tree --name-only -r ", (hash commit)]
          treeLines tree = filter (not . T.null) $ T.lines tree
          treeLength tree = length $ treeLines tree
          handleLeft exitCode = error $ show exitCode

-- Filter a git tree (given as a list of file paths) to exclude file paths that are not valid under the given glob
-- patterns.
treeGlobFilter' :: (EmIssueCommit, UC.Config, [T.Text]) -> (EmIssueCommit, [T.Text])
treeGlobFilter' (commit, config, files) = treeGlobFilter includePatterns excludePatterns extensionsPatterns (commit, files)
    where includePatterns    = UC.fpInclude filePatterns
          excludePatterns    = UC.fpExclude filePatterns
          extensionsPatterns = UC.fpExtensions filePatterns
          filePatterns       = UC.anaFilePatterns . UC.annAnalysis $ UC.confAnnotations config !! 0

treeGlobFilter :: [T.Text] -> [T.Text] -> [T.Text] -> (EmIssueCommit, [T.Text]) -> (EmIssueCommit, [T.Text])
treeGlobFilter includePatterns excludePatterns extensionsPatterns (commit, files) = (commit, filteredFiles)
          -- Convert list of text patterns into glob Patterns.
    where includeGlobPatterns    = map (compile . T.unpack) includePatterns
          excludeGlobPatterns    = map (compile . T.unpack) excludePatterns
          extensionsGlobPatterns = map (compile . T.unpack) $ map (\x -> T.concat ["**/*.", x]) extensionsPatterns

          -- Run all filters.
          includeFilteredFiles    = filter (fileMatchesPatterns includeGlobPatterns) files
          excludeFilteredFiles    = filter (fileDoesNotMatchPatterns excludeGlobPatterns) files
          extensionsFilteredFiles = filter (fileMatchesPatterns extensionsGlobPatterns) files

          -- Get the intersection of all filters.
          filteredFiles = L.intersect includeFilteredFiles $ L.intersect excludeFilteredFiles extensionsFilteredFiles

          -- Prepare the result as required in tuples.
          zipWithCommit commitRecord filesList = [(commitRecord, file) | file <- filesList]


-- Functions for internal use.

-- Determine if a file path matches a list of glob patterns.
fileMatchesPatterns :: [Pattern] -> T.Text -> Bool
fileMatchesPatterns [] _ = True
fileMatchesPatterns patterns file = fileMatchesPatterns' patterns file

fileMatchesPatterns' :: [Pattern] -> T.Text -> Bool
fileMatchesPatterns' [] _ = False
fileMatchesPatterns' (x:xs) file
    | isMatch == True  = True
    | isMatch == False = fileMatchesPatterns' xs file
    where isMatch = match x $ T.unpack file

-- Determine if a file path does not match a list of glob patterns.
fileDoesNotMatchPatterns :: [Pattern] -> T.Text -> Bool
fileDoesNotMatchPatterns [] _ = True
fileDoesNotMatchPatterns patterns file = fileDoesNotMatchPatterns' patterns file

fileDoesNotMatchPatterns' :: [Pattern] -> T.Text -> Bool
fileDoesNotMatchPatterns' [] file = True
fileDoesNotMatchPatterns' (x:xs) file
    | isMatch == True  = False
    | isMatch == False = fileDoesNotMatchPatterns' xs file
    where isMatch = match x $ T.unpack file
