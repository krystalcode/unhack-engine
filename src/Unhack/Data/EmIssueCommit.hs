{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Data.EmIssueCommit
       ( fromCommits
       , EmIssueCommit(..)
       ) where


-- Imports.

-- Internal dependencies.

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Time        (UTCTime)
import GHC.Generics     (Generic)

import qualified Data.Text as T (Text)

-- Internal dependencies.

import qualified Unhack.Commit as UC


-- Public API.

data EmIssueCommit = EmIssueCommit
    { _id  :: T.Text
    , hash :: T.Text
    , time :: UTCTime
    } deriving (Generic, Show)

fromCommits :: [(T.Text, UC.Commit)] -> [EmIssueCommit]
fromCommits commits = map fromCommit commits


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
