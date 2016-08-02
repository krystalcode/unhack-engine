{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Data.Project
       ( bulkUpdateRepositories
       , get
       , mgetActiveByRepositoryId
       ) where


-- Imports.

-- External dependencies.

import Data.Aeson          ((.=), eitherDecode, object, ToJSON)
import Data.Maybe          (fromJust, isJust, isNothing)
import Database.Bloodhound
import GHC.Generics        (Generic)
import Network.HTTP.Client

import qualified Data.Map  as M (fromList, map, toList, Map)
import qualified Data.Text as T (unpack, Text)

-- Internal dependencies.

import qualified Unhack.Data.EmProjectRepository              as UDEPR (EmProjectRepository)
import qualified Unhack.Data.Project                          as UDP
import qualified Unhack.Storage.ElasticSearch.Config          as USEC  (indexSettingsFromConfig, StorageConfig, StorageIndexSettings)
import qualified Unhack.Storage.ElasticSearch.Data.Repository as USEDR (get)
import qualified Unhack.Storage.ElasticSearch.Operations      as USEO  (bulkUpdateDocuments', getDocument', search', searchDefaultParams, SearchParams(..))


-- Public API.

-- Bulk update the repositories field for projects.
-- At the same we recalculate and update the project's build status, because we always want to keep
-- it up to date if the included repositories or their build statuses change.
bulkUpdateRepositories :: USEC.StorageConfig -> M.Map DocId (Maybe [UDEPR.EmProjectRepository]) -> IO (Reply)
bulkUpdateRepositories storageConfig mEmRepositoriesWithProjectsIds = USEO.bulkUpdateDocuments' storageConfig indexSettings patches

    where patches       = M.toList $ M.map (\emRepositories -> Repositories (UDP.combineBuilds emRepositories) emRepositories) mEmRepositoriesWithProjectsIds
          indexSettings = projectIndexSettings storageConfig

-- Get a project record given its ID.
get :: USEC.StorageConfig -> DocId -> IO (Maybe UDP.Project)
get storageConfig projectId = do
    -- Get the project from Elastic Search.
    response <- USEO.getDocument' storageConfig (projectIndexSettings storageConfig) projectId

    -- Get the result from the body of the response.
    let body         = responseBody response
    let eitherResult = eitherDecode body :: Either String (EsResult UDP.Project)
    let result       = either error id eitherResult

    -- Get the project from the _source attribute of the result.
    let maybeProject = fmap _source . foundResult $ result

    return maybeProject

-- Get active project records that have the given repository as one of the included in the project.
mgetActiveByRepositoryId :: USEC.StorageConfig -> T.Text -> IO (Maybe (M.Map DocId UDP.Project))
mgetActiveByRepositoryId storageConfig repositoryId = do

    -- Nested query that filters by repository id.
    let repositoryFilter    = TermQuery (Term "repositories._id" repositoryId) Nothing
    let repositoryBoolQuery = QueryBoolQuery (BoolQuery [] [] [] Nothing Nothing Nothing (Just [repositoryFilter]))
    let repositoryPath      = QueryPath "repositories"
    let repositoryQuery     = QueryNestedQuery (NestedQuery repositoryPath ScoreTypeNone repositoryBoolQuery)

    -- Query that filters by 'isDeleted' field.
    let isDeletedQuery = TermQuery (Term "isDeleted" "false") Nothing

    -- Top level query that combine both filters.
    let boolQuery = QueryBoolQuery (BoolQuery [isDeletedQuery, repositoryQuery] [] [] Nothing Nothing Nothing Nothing)
    let query     = ConstantScoreQuery boolQuery (Boost 1)

    response <- USEO.search' storageConfig (projectIndexSettings storageConfig) query (USEO.searchDefaultParams { USEO.spSize = Size 10000 })

    let body         = responseBody response
    let eitherResult = eitherDecode body :: Either String (SearchResult UDP.Project)
    let result       = either error id eitherResult
    let resultHits   = hits . searchHits $ result

    -- Create a Map with the projects' ids and records.
    let lMaybeProjectsWithIds = map (\hit -> (hitDocId hit, hitSource hit)) resultHits
    let lJustProjectsWithIds  = filter (\(projectId, project) -> isJust project) lMaybeProjectsWithIds
    let lProjectsWithIds      = map (\(projectId, project) -> (projectId, fromJust project)) lJustProjectsWithIds

    case length lProjectsWithIds of

        0 -> return Nothing

        _ -> return $ Just (M.fromList lProjectsWithIds)


-- Functions/types for internal use.

-- Patches required for the Update API.

-- The patch for updating the repositories field contains the build field as
-- well. This is because when we update the repositories of a project we always
-- want to recalculate its build status.
data Patch = Repositories { build :: Maybe UDP.Build, repositories :: Maybe [UDEPR.EmProjectRepository] }
             deriving (Show, Generic)

instance ToJSON Patch where
    toJSON (Repositories build repositories) =
        object [ "build"        .= build
               , "repositories" .= repositories ]


-- Functions/types for internal use.

-- Get the settings for the project index.
projectIndexSettings :: USEC.StorageConfig -> USEC.StorageIndexSettings
projectIndexSettings storageConfig = (USEC.indexSettingsFromConfig "project" storageConfig)
