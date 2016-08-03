{-# LANGUAGE OverloadedStrings #-}

module Unhack.Issue
    ( Issue(..)
    ) where


-- Imports.

-- External dependencies.

import Data.Aeson
import Data.Time  (UTCTime)

import qualified Data.Text as T (Text)

-- Internal dependencies.

import Unhack.Data.EmIssueCommit      (EmIssueCommit)
import Unhack.Data.EmbeddedRepository (EmbeddedRepository)
import Unhack.Data.IssueProperties    (IssueProperties)


-- Public API.

data Issue = Issue
    { commit     :: EmIssueCommit
    , createdAt  :: UTCTime
    {-
      @Issue(
        "Support path hierarchy in file property",
        type="improvement",
        priority="high"
      )
    -}
    , file       :: T.Text
    , properties :: IssueProperties
    , repository :: EmbeddedRepository
    , updatedAt  :: UTCTime
    } deriving (Show)


-- Functions for internal use.

-- JSON conversions.
instance ToJSON Issue where
    toJSON (Issue commit createdAt file properties repository updatedAt) =
        object [ "commit"     .= commit
               , "createdAt"  .= createdAt
               , "file"       .= file
               , "properties" .= properties
               , "repository" .= repository
               , "updatedAt"  .= updatedAt
               ]
