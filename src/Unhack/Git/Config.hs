{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Config ( remoteUrl ) where

import qualified Data.Text as T (Text)
import Unhack.Process

-- Public API

remoteUrl :: FilePath -> IO (T.Text)
remoteUrl directory = lazyProcess "git config --get remote.origin.url" directory
