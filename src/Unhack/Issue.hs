{-# LANGUAGE OverloadedStrings #-}

module Unhack.Issue
    ( makeIssue
    , Issue(..)
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
    , type'      :: T.Text
    , updatedAt  :: UTCTime
    } deriving (Show)

-- Function for more easily creating an Issue without providing optional fields
-- or fields that have default values.
makeIssue :: EmIssueCommit -> T.Text -> IssueProperties -> EmbeddedRepository -> UTCTime -> Issue
makeIssue commit file properties repository now = Issue
    { commit     = commit
    , createdAt  = now
    , file       = file
    , properties = properties
    , repository = repository
    , type'      = "Issue"
    , updatedAt  = now
    }


-- Functions for internal use.

-- JSON conversions.
instance ToJSON Issue where
    toJSON (Issue commit createdAt file properties repository type' updatedAt) =
        object [ "commit"     .= commit
               , "createdAt"  .= createdAt
               , "file"       .= file
               , "properties" .= properties
               , "repository" .= repository
               , "type"       .= type'
               , "updatedAt"  .= updatedAt
               ]
