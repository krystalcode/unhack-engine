{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Data.GitCommit
    ( fromText
    , GitCommit(..)
    ) where


-- Imports.

-- External dependencies.

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Time        (UTCTime)
import GHC.Generics     (Generic)

import qualified Data.Text as T (splitOn, Text)

-- Internal dependencies.

import Unhack.Util (posixToUTC)


-- Public API.

data GitCommit = GitCommit
    { hash :: T.Text
    , time :: UTCTime
    } deriving (Generic, Show)

-- Converts text to a GitCommit record.
-- The text needs to be in the following format {hash}_{unix_timestamp}. For example:
-- b298cff142193c444af4bf4c4ec68f618396c059_1453917211
fromText :: T.Text -> GitCommit
fromText commitText = GitCommit { hash = commitList !! 0
                                , time = posixToUTC $ commitList !! 1 }
    where commitList = T.splitOn "_" commitText
