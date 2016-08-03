{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Data.Repository
       ( get
       , markAccessible
       , markProcessed
       , updateHeadCommits
       ) where


-- Imports.

-- External dependencies.

import Data.Aeson          ((.=), eitherDecode, object, ToJSON)
import Data.Time           (UTCTime)
import Database.Bloodhound
import GHC.Generics        (Generic)
import Network.HTTP.Client

import qualified Data.Text as T (Text)

-- Internal dependencies.

import qualified Unhack.Data.EmCommit                    as UDEC (EmCommit)
import qualified Unhack.Data.Repository                  as UDR
import qualified Unhack.Storage.ElasticSearch.Config     as USEC (indexSettingsFromConfig, StorageConfig, StorageIndexSettings)
import qualified Unhack.Storage.ElasticSearch.Operations as USEO (bulkUpdateDocuments', getDocument', updateDocument')


-- Public API.

-- Get a repository record given its ID.
get :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> IO (Maybe UDR.Repository)
get config indexSettings repositoryId = do
    -- Get the repository from Elastic Search.
    response <- USEO.getDocument' config indexSettings (DocId repositoryId)

    -- Get the result from the body of the response.
    let body         = responseBody response
    let eitherResult = eitherDecode body :: Either String (EsResult UDR.Repository)
    let result       = either error id eitherResult

    -- Get the repository from the _source attribute of the result.
    let maybeRepository = fmap _source . foundResult $ result

    return maybeRepository

{-
  @Issue(
    "Update flag fields in records using the Update API instead of submitting the whole document again"
    type="improvement"
    priority="low"
  )
-}
-- Set the 'isAccessible' flag to true for the given repository.
markAccessible :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> UDR.Repository -> UTCTime -> IO (Reply)
markAccessible config indexSettings repositoryId repository now = USEO.updateDocument' config indexSettings updatedRepository (DocId repositoryId)
    where updatedRepository = repository { UDR.isAccessible = True, UDR.updatedAt = now }

-- Set the 'isProcessed' flag to true for the given repository.
markProcessed :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> UDR.Repository -> UTCTime -> IO (Reply)
markProcessed config indexSettings repositoryId repository now = USEO.updateDocument' config indexSettings updatedRepository (DocId repositoryId)
    where updatedRepository = repository { UDR.isProcessed = True, UDR.updatedAt = now }

-- Update the 'headCommit' fields for multiple repositories.
updateHeadCommits :: USEC.StorageConfig -> [(T.Text, UDEC.EmCommit)] -> UTCTime -> IO (Reply)
updateHeadCommits storageConfig repositoriesIdsWithEmCommits now = USEO.bulkUpdateDocuments' storageConfig indexSettings $ patches now

    where patches now   = map (\(repositoryId, repositoryHeadCommit) -> (DocId repositoryId, HeadCommit repositoryHeadCommit now)) repositoriesIdsWithEmCommits
          indexSettings = USEC.indexSettingsFromConfig "repository" storageConfig


-- Functions/types for internal use.

-- Patches required for the Update API.

data Patch = HeadCommit { headCommit :: UDEC.EmCommit, updatedAt :: UTCTime }
             deriving (Show, Generic)

instance ToJSON Patch where
    toJSON (HeadCommit headCommit updatedAt) =
        object [ "headCommit" .= headCommit
               , "updatedAt"  .= updatedAt
               ]
