{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Fetch
       ( fetch
       , fetchBranch ) where

import qualified Data.Text as T (concat, Text)
import Unhack.Process

-- Public API

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
