{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Commit
       ( bulkSetRepositoryId
       , emptyCommit
       , Commit(..)
       ) where


-- Imports.

-- External dependencies.

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GHC.Generics     (Generic)

import qualified Data.Text as T (Text)

-- Internal dependencies.

import qualified Unhack.Data.EmBranch as UDEB (EmBranch)


-- Public API.

data Commit = Commit { repositoryId :: T.Text
                     , hash         :: T.Text
                     , time         :: T.Text
                     , branches     :: Maybe [UDEB.EmBranch]
                     , buildStatus  :: T.Text
                     , buildMessage :: T.Text
                     , isProcessed    :: Bool
                     } deriving (Generic, Show)

emptyCommit = Commit { repositoryId = ""
                     , hash         = ""
                     , time         = ""
                     , branches     = Nothing
                     , buildStatus  = ""
                     , buildMessage = ""
                     , isProcessed  = False }

bulkSetRepositoryId :: [Commit] -> T.Text -> [Commit]
bulkSetRepositoryId commits repositoryId = map (\commit -> commit { repositoryId = repositoryId }) commits


-- Functions/types for internal use.

instance FromJSON Commit where
    parseJSON (Object v) = Commit
                           <$> v .:  "repositoryId"
                           <*> v .:  "hash"
                           <*> v .:  "time"
                           <*> v .:? "branches"     .!= Nothing
                           <*> v .:  "buildStatus"
                           <*> v .:  "buildMessage"
                           <*> v .:  "isProcessed"
    parseJSON invalid    = typeMismatch "Commit" invalid

instance ToJSON Commit where
    toJSON (Commit repositoryId hash time branches buildStatus buildMessage isProcessed) =
        object [ "repositoryId" .= repositoryId
               , "hash"         .= hash
               , "time"         .= time
               , "branches"     .= branches
               , "buildStatus"  .= buildStatus
               , "buildMessage" .= buildMessage
               , "isProcessed"  .= isProcessed ]
