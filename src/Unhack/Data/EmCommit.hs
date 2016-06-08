{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Data.EmCommit
       ( emptyEmCommit
       , toCommits
       , EmCommit(..)
       ) where


-- Imports.

-- External dependencies.

import Data.Aeson
import Data.Aeson.Types          (typeMismatch)
import Database.Bloodhound.Types (DocId(..))
import GHC.Generics              (Generic)

import qualified Data.Text as T (Text)

-- Internal dependencies.

import qualified Unhack.Commit as UC


-- Public API.

data EmCommit = EmCommit
    { _id          :: DocId
    , hash         :: T.Text
    , time         :: T.Text
    , buildStatus  :: T.Text
    , buildMessage :: T.Text } deriving (Generic, Show)

emptyEmCommit :: EmCommit
emptyEmCommit = EmCommit
    { _id          = DocId ""
    , hash         = ""
    , time         = ""
    , buildStatus  = ""
    , buildMessage = ""}

toCommits :: [EmCommit] -> T.Text -> [UC.Commit]
toCommits emCommits repositoryId = map mapCommit emCommits
    where mapCommit emCommit = UC.emptyCommit { UC.repositoryId = repositoryId
                                              , UC.hash         = hash emCommit
                                              , UC.time         = time emCommit
                                              , UC.buildStatus  = buildStatus emCommit
                                              , UC.buildMessage = buildMessage emCommit }


-- Functions/types for internal use.

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
