{-# LANGUAGE OverloadedStrings #-}

module Unhack.Pubsub.Repository.Delete
    ( delete
    ) where


-- Imports.

-- External dependencies.

import Database.Bloodhound.Types (BoolQuery(..), Boost(..), DocId(..), NestedQuery(..), ScoreType(..), Term(..), Query(..), QueryPath(..))

-- Internal dependencies.

import qualified Unhack.Data.Repository                  as UDR  (Repository)
import qualified Unhack.Storage.ElasticSearch.Config     as USEC (StorageConfig)
import qualified Unhack.Storage.ElasticSearch.Operations as USEO (deleteByQuery', deleteDocument')
import qualified Unhack.Storage.ElasticSearch.Types      as USET (DocType(..))


-- Public API.

-- Delete a Repository record, given its id, and all the Branch, Commit and
-- Issue records that belong to it.
{-
  @Issue(
    "Looks for errors in the responses and log a message"
    type="bug"
    priority="low"
    labels="data, error management"
  )
  @Issue(
    "Use the 'conflicts=proceed' options to delete records in the unlikely event
    where there are version conflicts"
    type="bug"
    priority="low"
    labels="data"
  )
-}
delete :: USEC.StorageConfig -> DocId -> IO ()
delete storageConfig documentId@(DocId repositoryId) = do

    -- Delete all Branch and Commit records that belong to the repository.
    -- Term query that filters by the 'repositoryId' field, which is the case
    -- for Branch and Commit records.
    let termQuery = TermQuery (Term "repositoryId" repositoryId) Nothing
    let bcQuery   = ConstantScoreQuery termQuery (Boost 1)

    bcResponse <- USEO.deleteByQuery' storageConfig [USET.Branch, USET.Commit] bcQuery

    -- Delete all Issue records that belong to the repository.
    -- Nested query that filters by the 'repository._id' field, which is the case for Issue records.
    let nestedFilter    = TermQuery (Term "repository._id" repositoryId) Nothing
    let nestedBoolQuery = QueryBoolQuery (BoolQuery [] [] [] Nothing Nothing Nothing (Just [nestedFilter]))
    let nestedPath      = QueryPath "repository"
    let nestedQuery     = QueryNestedQuery (NestedQuery nestedPath ScoreTypeNone nestedBoolQuery)
    let iQuery          = ConstantScoreQuery nestedQuery (Boost 1)

    iResponse <- USEO.deleteByQuery' storageConfig [USET.Issue] iQuery

    -- Delete the repository itself.
    rResponse <- USEO.deleteDocument' storageConfig USET.Repository documentId

    return mempty
