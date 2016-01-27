{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Commit
       ( Commit(..)
       , emptyCommit
       , textToCommit
       ) where

import Data.Aeson
import qualified Data.Text as T (concat, filter, splitAt, splitOn, unpack, Text)
import GHC.Generics (Generic)


-- Public API.

data Commit = Commit { hash :: T.Text
                     , time :: T.Text
                     } deriving (Generic, Show)

emptyCommit = Commit { hash = ""
                     , time = "" }

-- Converts text to a Commit record.
-- The text needs to be in one of the following supported formats:
-- - "hash_iso_strict": The commit's hash and time in strict ISO 8601 format
--   separated by underscore. For example:
--   75526bac404a78ac1d56e85fa5919d68ee15df2b_2016-01-24T05:45:00+00:00.
-- - "hash_unix_timestamp": The commit's hash and time in unix timestamp format,
--   separated by underscore. For example:
--   b298cff142193c444af4bf4c4ec68f618396c059_1453917211
textToCommit :: T.Text -> T.Text -> Commit
textToCommit commitText "hash_iso_strict" = Commit { hash = commitHash
                                                   , time = commitTime }
    where commitList = T.splitOn "_" commitText
          commitHash = commitList !! 0
          dateParts = T.splitOn "+" $ commitList !! 1
          commitTime = T.concat [T.filter (not . (`elem` ['-', ':'])) $ dateParts !! 0, "+", dateParts !! 1]

textToCommit commitText "hash_unix_timestamp" = Commit { hash = commitList !! 0
                                                       , time = commitList !! 1 }
    where commitList = T.splitOn "_" commitText

textToCommit commitText format = error . T.unpack $ T.concat ["The requested text format \"", format, "\" is not supported for converting text to a Commit record."]


-- Functions/types for internal use.

instance FromJSON Commit

instance ToJSON Commit where
    toJSON (Commit hash time) =
        object [ "hash" .= hash
               , "time" .= time ]
