{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Commit
       ( bulkSetRepositoryId
       , emptyCommit
       , Commit(..)
       ) where


-- Imports.

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T (Text)
import GHC.Generics (Generic)


-- Public API.

data Commit = Commit { repositoryId :: T.Text
                     , hash         :: T.Text
                     , time         :: T.Text
                     , buildStatus  :: T.Text
                     , buildMessage :: T.Text
                     } deriving (Generic, Show)

emptyCommit = Commit { repositoryId = ""
                     , hash         = ""
                     , time         = ""
                     , buildStatus  = ""
                     , buildMessage = "" }

bulkSetRepositoryId :: [Commit] -> T.Text -> [Commit]
bulkSetRepositoryId commits repositoryId = map (\commit -> commit { repositoryId = repositoryId }) commits


-- Functions/types for internal use.

instance FromJSON Commit where
    parseJSON (Object v) = Commit
                           <$> v .: "repositoryId"
                           <*> v .: "hash"
                           <*> v .: "time"
                           <*> v .: "buildStatus"
                           <*> v .: "buildMessage"
    parseJSON invalid    = typeMismatch "Commit" invalid

instance ToJSON Commit where
    toJSON (Commit repositoryId hash time buildStatus buildMessage) =
        object [ "repositoryId" .= repositoryId
               , "hash"         .= hash
               , "time"         .= time
               , "buildStatus"  .= buildStatus
               , "buildMessage" .= buildMessage ]
