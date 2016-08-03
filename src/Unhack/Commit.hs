{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Commit
       ( bulkSetRepositoryId
       , makeCommit
       , Commit(..)
       ) where


-- Imports.

-- External dependencies.

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Time        (UTCTime)
import GHC.Generics     (Generic)

import qualified Data.Text as T (Text)

-- Internal dependencies.

import qualified Unhack.Data.EmBranch as UDEB (EmBranch)


-- Public API.

data Commit = Commit { branches     :: Maybe [UDEB.EmBranch]
                     , buildMessage :: T.Text
                     , buildStatus  :: T.Text
                     , createdAt    :: UTCTime
                     , hash         :: T.Text
                     , isProcessed  :: Bool
                     , repositoryId :: T.Text
                     , time         :: UTCTime
                     , updatedAt    :: UTCTime
                     } deriving (Generic, Show)

-- Helper function for creating a Commit record by providing only the most
-- essential fields.
makeCommit :: Maybe [UDEB.EmBranch] -> T.Text -> T.Text -> UTCTime -> UTCTime -> Commit
makeCommit branches hash repositoryId time now
    = Commit
        { branches     = branches
        , buildMessage = ""
        , buildStatus  = ""
        , createdAt    = now
        , hash         = hash
        , isProcessed  = False
        , repositoryId = repositoryId
        , time         = time
        , updatedAt    = now
        }

bulkSetRepositoryId :: [Commit] -> T.Text -> [Commit]
bulkSetRepositoryId commits repositoryId = map (\commit -> commit { repositoryId = repositoryId }) commits


-- Functions/types for internal use.

instance FromJSON Commit where
    parseJSON (Object v) = Commit
                           <$> v .:? "branches"     .!= Nothing
                           <*> v .:  "buildMessage"
                           <*> v .:  "buildStatus"
                           <*> v .:  "createdAt"
                           <*> v .:  "hash"
                           <*> v .:  "isProcessed"
                           <*> v .:  "repositoryId"
                           <*> v .:  "time"
                           <*> v .:  "updatedAt"
    parseJSON invalid    = typeMismatch "Commit" invalid

instance ToJSON Commit where
    toJSON (Commit branches buildMessage buildStatus createdAt hash isProcessed repositoryId time updatedAt) =
        object [ "branches"     .= branches
               , "buildMessage" .= buildMessage
               , "buildStatus"  .= buildStatus
               , "createdAt"    .= createdAt
               , "hash"         .= hash
               , "isProcessed"  .= isProcessed
               , "repositoryId" .= repositoryId
               , "time"         .= time
               , "updatedAt"    .= updatedAt
               ]
