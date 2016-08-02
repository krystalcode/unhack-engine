{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Unhack.Git.Branch
       ( branchesList
       , originList
       ) where


-- Imports.

-- External dependencies.

import Data.Maybe (fromJust, isJust)

import qualified Data.Text as T (filter, lines, stripPrefix, words, Text)

-- Internal dependencies.

import Unhack.Data.Branch
import Unhack.Process


-- Public API.

-- Gets the list of branches from the remote repository.
originList :: FilePath -> IO ([T.Text])
originList directory = do
    -- Get the list of branches from the origin as provided by git (text).
    text <- lazyProcess "git ls-remote --heads origin" directory

    -- Split them by new line and remove the hash so that we get only the names.
    let list  = map T.words $ T.lines text
    let names = map (flip (!!) $ 1) list

    -- Remove the 'refs/heads/' prefix from the names.
    let maybeWithoutPrefixes = map (T.stripPrefix "refs/heads/") names
    let justWithoutPrefixes  = filter isJust maybeWithoutPrefixes

    return $ map fromJust justWithoutPrefixes

-- Gets the list of branches as text by executing the "git branch" command. The
-- branches are those that local git knows about and may not be up-to-date.
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
