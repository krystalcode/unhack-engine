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
-- - "git_log": Text returned by the "git log" command in the format used in
--   Unhack.Git.Commit.
textToCommit :: T.Text -> T.Text -> Commit
textToCommit commitText "git_log" = Commit { hash = commitList !! 0
                                                 -- Convert to ISO 8601.
                                               , time = T.concat [day, "T", timeAndTimezone] }
    where commitList = T.splitOn "_" commitText
          date = T.filter (/= ' ') $ commitList !! 1
          (day, timeAndTimezone) = T.splitAt 10 date
textToCommit commitText format = error . T.unpack $ T.concat ["The requested text format \"", format, "\" is not supported for converting text to a Commit record."]


-- Functions/types for internal use.

instance FromJSON Commit

instance ToJSON Commit where
    toJSON (Commit hash time) =
        object [ "hash" .= hash
               , "time" .= time ]
