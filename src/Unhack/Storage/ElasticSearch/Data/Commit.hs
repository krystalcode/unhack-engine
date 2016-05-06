{-# LANGUAGE OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Data.Commit
       ( get
       , setBuildStatus )
       where


-- Imports.

-- External dependencies.

import           Data.Aeson               (eitherDecode)
import qualified Data.Text           as T (Text)
import           Database.Bloodhound
import           Network.HTTP.Client

-- Internal dependencies.

import qualified Unhack.Commit                           as UDC  (Commit(..))
import qualified Unhack.Storage.ElasticSearch.Config     as USEC (StorageConfig, StorageIndexSettings)
import qualified Unhack.Storage.ElasticSearch.Operations as USEO (getDocument', updateDocument')


-- Public API.

-- Get a commit record given its ID.
get :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> IO (Maybe UDC.Commit)
get config indexSettings commitId = do
    -- Get the commit from Elastic Search.
    response <- USEO.getDocument' config indexSettings (DocId commitId)

    -- Get the result from the body of the response.
    let body         = responseBody response
    let eitherResult = eitherDecode body :: Either String (EsResult UDC.Commit)
    let result       = either error id eitherResult

    -- Get the commit from the _source attribute of the result.
    let maybeCommit = fmap _source . foundResult $ result

    return maybeCommit

-- Set the 'buildStatus' for a commit to the given value.
setBuildStatus :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> UDC.Commit -> T.Text -> IO (Reply)
setBuildStatus config indexSettings commitId commit buildStatus = USEO.updateDocument' config indexSettings updatedCommit (DocId commitId)
    where updatedCommit = commit { UDC.buildStatus = buildStatus }
