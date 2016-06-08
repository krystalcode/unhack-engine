{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Fetch
       ( clone
       , fetch ) where


-- Imports.

-- External dependencies.

import System.Directory (createDirectoryIfMissing)

import qualified Data.Text as T (concat, intercalate, unpack, Text)

-- Interal dependencies.

import Unhack.Process

import qualified Unhack.Git.Location as UGL (base)

-- Public API.

{-
    @Issue(
        "Add a function that does a clone or fetch depending on whether the
        repository is already cloned"
        type="improvement"
        priority="low"
    )
-}

-- Clone a git repository.
clone :: T.Text -> T.Text -> T.Text -> IO (T.Text)
clone vendor owner repository = do
    createDirectoryIfMissing True directory
    lazyProcess command directory
    where command   = T.concat ["git clone --no-checkout ", url, " ", repository]
          directory = T.unpack $ T.intercalate "/" [UGL.base, vendor, owner]
          url       = case vendor of

                          "bitbucket" -> T.concat ["git@bitbucket.org:", owner, "/", repository, ".git"]

                          _           -> error $ "Unsupported vendor '" ++ (T.unpack vendor) ++ "' while cloning the repository '" ++ (T.unpack repository)

-- Fetch multiple branches from origin.
fetch :: FilePath -> [T.Text] -> IO (T.Text)
fetch directory branches = do
    fetch' directory options

    where options       = T.concat ["--update-head-ok ", "origin ", branchesPairs]
          branchesPairs = T.intercalate " " $ map (\branch -> T.concat [branch, ":", branch]) branches


-- Functions/types for internal use.

{-
    @Issue(
        "Successful fetch outputs the result as an empty string"
        type="bug"
        priority="low"
    )
-}

-- Execute a 'git fetch' command with the given options.
fetch' :: FilePath -> T.Text -> IO (T.Text)
fetch' directory options = lazyProcess command directory
    where command = T.concat ["git fetch ", options]
