{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Unhack.Git.Branch
       ( branchesList
       , branchesRecords
       , listToRecords
       ) where


-- Imports.

-- External dependencies.

import qualified Data.Text as T (filter, lines, stripPrefix, Text)

-- Internal dependencies.

import Unhack.Data.Branch
import Unhack.Process


-- Public API.

-- Gets the list of commits as text by executing the "git log" command for the
-- requested branch.
{-
    @Issue(
        "Remove the origin/HEAD -> origin/master from the list of branches"
        type="bug"
        priority="low"
    )
-}
branchesList :: FilePath -> IO ([T.Text])
branchesList directory = do
    originBranchesText <- lazyProcess command directory
    let originBranchesList = textToList originBranchesText
    return $ removeOrigins originBranchesList
    where command = "git branch -r --no-color"
          textToList branches = T.lines $ T.filter (/= ' ') branches
          removeOrigins branches = map removeOrigin branches
          removeOrigin (T.stripPrefix "origin/" -> Just branchName) = branchName

branchesRecords :: FilePath -> T.Text -> IO ([Branch])
branchesRecords directory repositoryId = do
    branchesAsList <- branchesList directory
    return $ map (\branchName -> emptyBranch { repositoryId = repositoryId, name = branchName }) branchesAsList

listToRecords :: [T.Text] -> [Branch]
listToRecords branchesAsList = map (\branchName -> emptyBranch { name = branchName }) branchesAsList
