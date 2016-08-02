{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Fetch
       ( clone
       , fetch ) where


-- Imports.

-- External dependencies.

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)

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
    -- Create the parent directory, if it does not exist.
    -- The parent directory is named '/repositories/vendor/owner' and it needs
    -- to be created before we try to clone the repository into it.
    createDirectoryIfMissing True directory

    -- Check if the repository directory exists. If it exists, it means that the
    -- repository has already been cloned and there is nothing to do here.
    directoryExists <- doesDirectoryExist $ concat [directory, "/", T.unpack repository]

    case directoryExists of

        True  -> return "Repository already exists"

        False -> do
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
