{-# LANGUAGE OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Data.Repository
       ( get
       , markAccessible
       , markProcessed )
       where


-- Imports.

-- External dependencies.

import Data.Aeson          (eitherDecode, json, Value)
import Database.Bloodhound
import Network.HTTP.Client

import qualified Data.Text as T (Text)

-- Internal dependencies.

import qualified Unhack.Data.Repository                  as UDR
import qualified Unhack.Storage.ElasticSearch.Config     as USEC (StorageConfig, StorageIndexSettings)
import qualified Unhack.Storage.ElasticSearch.Operations as USEO (getDocument', updateDocument')


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

-- Set the 'isAccessible' flag to true for the given repository.
markAccessible :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> UDR.Repository -> IO (Reply)
markAccessible config indexSettings repositoryId repository = USEO.updateDocument' config indexSettings updatedRepository (DocId repositoryId)
    where updatedRepository = repository { UDR.isAccessible = True }

-- Set the 'isProcessed' flag to true for the given repository.
markProcessed :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> UDR.Repository -> IO (Reply)
markProcessed config indexSettings repositoryId repository = USEO.updateDocument' config indexSettings updatedRepository (DocId repositoryId)
    where updatedRepository = repository { UDR.isProcessed = True }
