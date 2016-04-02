{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Fetch
       ( clone
       , fetch
       , fetchBranch ) where


-- Imports.

import Control.Monad (unless)
import qualified Data.Text as T (concat, intercalate, unpack, Text)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import qualified Unhack.Git.Location as UGL (base)
import Unhack.Process

-- Public API

{-
    @Issue(
        "Add a function that does a clone or fetch depending on whether the
        repository is already cloned"
        type="improvement"
        priority="low"
    )
-}

clone :: T.Text -> T.Text -> T.Text -> IO (T.Text)
clone vendor owner repository = do
    createDirectoryIfMissing True directory
    lazyProcess command directory
    where command   = T.concat ["git clone --no-checkout ", url, " ", repository]
          directory = T.unpack $ T.intercalate "/" [UGL.base, vendor, owner]
          url       = case vendor of

                          "bitbucket" -> T.concat ["git@bitbucket.org:", owner, "/", repository, ".git"]

                          _           -> error $ "Unsupported vendor '" ++ (T.unpack vendor) ++ "' while cloning the repository '" ++ (T.unpack repository)

{-
    @Issue(
        "Successful fetch outputs the result as an empty string"
        type="bug"
        priority="low"
    )
-}

fetch :: FilePath -> T.Text -> IO (T.Text)
fetch directory options = lazyProcess command directory
    where command = T.concat ["git fetch ", options]

fetchBranch :: FilePath -> T.Text -> IO (T.Text)
fetchBranch directory branch = fetch directory $ T.concat ["origin ", branch]
