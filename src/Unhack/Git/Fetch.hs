module Unhack.Git.Fetch
       ( fetch
       , fetchBranch ) where

import Unhack.Process

-- Public API

{-
    @Issue(
        "Successful fetch outputs the result as an empty string"
        type="bug"
        priority="low"
    )
-}

fetch :: FilePath -> String -> IO (String)
fetch directory options = lazyProcess command directory
    where command = "git fetch " ++ options

fetchBranch :: FilePath -> String -> IO (String)
fetchBranch directory branch = fetch directory $ "origin " ++ branch
