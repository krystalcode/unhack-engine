{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Data.EmIssueCommit
       ( emptyEmIssueCommit
       , textToEmIssueCommit
       , fromCommits
       , EmIssueCommit(..)
       ) where


-- Imports.

-- Internal dependencies.

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GHC.Generics     (Generic)

import qualified Data.Text as T (concat, filter, splitOn, unpack, Text)

-- Internal dependencies.

import qualified Unhack.Commit as UC


-- Public API.

data EmIssueCommit = EmIssueCommit
    { _id  :: T.Text
    , hash :: T.Text
    , time :: T.Text } deriving (Generic, Show)

emptyEmIssueCommit :: EmIssueCommit
emptyEmIssueCommit = EmIssueCommit
    { _id  = ""
    , hash = ""
    , time = "" }

fromCommits :: [(T.Text, UC.Commit)] -> [EmIssueCommit]
fromCommits commits = map fromCommit commits

-- Converts text to a Commit record.
-- The text needs to be in one of the following supported formats:
-- - "hash_iso_strict": The commit's hash and time in strict ISO 8601 format
--   separated by underscore. For example:
--   75526bac404a78ac1d56e85fa5919d68ee15df2b_2016-01-24T05:45:00+00:00.
-- - "hash_unix_timestamp": The commit's hash and time in unix timestamp format,
--   separated by underscore. For example:
--   b298cff142193c444af4bf4c4ec68f618396c059_1453917211
textToEmIssueCommit :: T.Text -> T.Text -> EmIssueCommit
textToEmIssueCommit commitText "hash_iso_strict" = emptyEmIssueCommit { hash = commitHash
                                                                      , time = commitTime }
    where commitList = T.splitOn "_" commitText
          commitHash = commitList !! 0
          dateParts = T.splitOn "+" $ commitList !! 1
          commitTime = T.concat [T.filter (not . (`elem` ['-', ':'])) $ dateParts !! 0, "+", dateParts !! 1]

textToEmIssueCommit commitText "hash_unix_timestamp" = emptyEmIssueCommit { hash = commitList !! 0
                                                                          , time = commitList !! 1 }
    where commitList = T.splitOn "_" commitText

textToEmIssueCommit commitText format = error . T.unpack $ T.concat ["The requested text format \"", format, "\" is not supported for converting text to a Commit record."]


-- Functions/types for internal use.

fromCommit :: (T.Text, UC.Commit) -> EmIssueCommit
fromCommit (commitId, commit) = EmIssueCommit { _id  = commitId
                                              , hash = UC.hash commit
                                              , time = UC.time commit }

instance FromJSON EmIssueCommit where
    parseJSON (Object v) = EmIssueCommit
                           <$> v .: "_id"
                           <*> v .: "hash"
                           <*> v .: "time"
    parseJSON invalid    = typeMismatch "EmIssueCommit" invalid

instance ToJSON EmIssueCommit where
    toJSON (EmIssueCommit _id hash time) =
        object [ "_id"  .= _id
               , "hash" .= hash
               , "time" .= time ]

instance Eq EmIssueCommit where
    (EmIssueCommit _id1 hash1 time1) == (EmIssueCommit _id2 hash2 time2) = _id1 == _id2 && hash1 == hash2 && time1 == time2
