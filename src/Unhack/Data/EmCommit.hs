{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Data.EmCommit
       ( fromCommits
       , EmCommit(..)
       ) where


-- Imports.

-- External dependencies.

import Data.Aeson
import Data.Aeson.Types          (typeMismatch)
import Data.Time                 (UTCTime)
import Database.Bloodhound.Types (DocId(..))
import GHC.Generics              (Generic)

import qualified Data.Text as T (Text)

-- Internal dependencies.

import qualified Unhack.Commit as UC


-- Public API.

data EmCommit = EmCommit
    { _id          :: DocId
    , hash         :: T.Text
    , time         :: UTCTime
    , buildStatus  :: T.Text
    , buildMessage :: T.Text } deriving (Generic, Show)

-- Get a list of EmCommit records from a list of Commit records with their IDs.
fromCommits :: [(DocId, UC.Commit)] -> [EmCommit]
fromCommits commits = map (\(commitId, commit) -> fromCommit commitId commit) commits


-- Functions/types for internal use.

-- Get an EmCommit record from a Commit record and it's ID.
fromCommit :: DocId -> UC.Commit -> EmCommit
fromCommit commitId commit = EmCommit { _id          = commitId
                                      , hash         = UC.hash commit
                                      , time         = UC.time commit
                                      , buildStatus  = UC.buildStatus commit
                                      , buildMessage = UC.buildMessage commit }

instance FromJSON EmCommit where
    parseJSON (Object v) = EmCommit
                           <$> v .: "_id"
                           <*> v .: "hash"
                           <*> v .: "time"
                           <*> v .: "buildStatus"
                           <*> v .: "buildMessage"
    parseJSON invalid    = typeMismatch "EmCommit" invalid

instance ToJSON EmCommit where
    toJSON (EmCommit _id hash time buildStatus buildMessage) =
        object [ "_id"          .= _id
               , "hash"         .= hash
               , "time"         .= time
               , "buildStatus"  .= buildStatus
               , "buildMessage" .= buildMessage ]
