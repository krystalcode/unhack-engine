{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Unhack.Git.Branch
       ( originList
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
