{-# LANGUAGE OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Types
    ( toIndex
    , toIndexSettings
    , toMapping
    , DocType(..)
    ) where


-- Imports.

-- External dependencies.

import Database.Bloodhound.Types (IndexName(..), MappingName)

import Data.Text as T (Text)

-- Internal dependencies.

import Unhack.Storage.ElasticSearch.Mappings.Branch     (branchMapping)
import Unhack.Storage.ElasticSearch.Mappings.Commit     (commitMapping)
import Unhack.Storage.ElasticSearch.Mappings.Issue      (issueMapping)
import Unhack.Storage.ElasticSearch.Mappings.Project    (projectMapping)
import Unhack.Storage.ElasticSearch.Mappings.Repository (repositoryMapping)

import qualified Unhack.Storage.ElasticSearch.Config as USEC (indexSettingsFromConfig, StorageConfig, StorageIndexSettings(..))


-- Public API.

data DocType = Branch | Commit | Issue | Project | Repository

-- Get the index name for the given document type.
toIndex :: USEC.StorageConfig -> DocType -> IndexName
toIndex storageConfig documentType = IndexName $ USEC.name (toIndexSettings storageConfig documentType)

-- Get the index settings for the given document type.
toIndexSettings :: USEC.StorageConfig -> DocType -> USEC.StorageIndexSettings
toIndexSettings storageConfig documentType = USEC.indexSettingsFromConfig (toText documentType) storageConfig

-- Get the mapping name for the given document type.
toMapping :: DocType -> MappingName
toMapping Branch     = branchMapping
toMapping Commit     = commitMapping
toMapping Issue      = issueMapping
toMapping Project    = projectMapping
toMapping Repository = repositoryMapping


-- Functions/types for internal use.

toText :: DocType -> T.Text
toText Branch     = "branch"
toText Commit     = "commit"
toText Issue      = "issue"
toText Project    = "project"
toText Repository = "repository"
