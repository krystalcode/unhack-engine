{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Config
       ( remoteUrl
       , remoteName
       , remoteVendor
       , remoteUrlToName
       , remoteUrlToVendor ) where


-- Imports.

-- External dependencies.

import qualified Data.Text as T (pack, replace, Text)

-- External dependencies.

import Unhack.Process

-- Public API

{-
    @Issue(
        "The Unhack.Git.Config module may not be needed anymore with the new
        flow of tasks"
        type="task"
        priority="low"
        labels="cleanup"
    )
-}

remoteUrl :: FilePath -> IO (T.Text)
remoteUrl directory = lazyProcess "git config --get remote.origin.url" directory

remoteName :: FilePath -> IO (T.Text)
remoteName directory = do
    url <- remoteUrl directory
    return $ remoteUrlToName url

remoteVendor :: FilePath -> IO (T.Text)
remoteVendor directory = do
    url <- remoteUrl directory
    return $ remoteUrlToVendor url

{-
    @Issue(
        "Improve the reliability of this function and support other vendors and protocols"
        type="bug"
        priority="low"
    )
-}
remoteUrlToName :: T.Text -> T.Text
remoteUrlToName url = withExtensionRemoved
    where withVendorParsed     = T.replace "git@bitbucket.org:" "bitbucket" url
          withExtensionRemoved = T.replace ".git" "" url

{-
    @Issue(
        "Implement this function with support for multiple vendors"
        type="bug"
        priority="low"
    )
-}
remoteUrlToVendor :: T.Text -> T.Text
remoteUrlToVendor url = T.pack "bitbucket"
