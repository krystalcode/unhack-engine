{-# LANGUAGE DeriveGeneric, RecordWildCards, OverloadedStrings #-}

module Unhack.Data.Project
       ( combineBuilds
       , updateBuild
       , Build(..)
       , Project(..)
       ) where


-- Imports.

-- External dependencies.

import Data.Aeson
import Data.Aeson.Types          (typeMismatch)
import Data.Time                 (UTCTime)
import Database.Bloodhound.Types (DocId)
import Data.Maybe                (fromJust, isJust)
import GHC.Generics              (Generic)

import qualified Data.Text as T (Text)

-- Internal dependences.

import Unhack.Types (EntityType)
import Unhack.Util  (omitNulls)

import qualified Unhack.Data.EmCommit            as UDEC  (EmCommit(..))
import qualified Unhack.Data.EmProjectRepository as UDEPR (EmProjectRepository(..))


-- Public API.

data Project = Project
    { build           :: Maybe Build
    , createdAt       :: UTCTime
    , isDeleted       :: Bool
    , name            :: T.Text
    , ownerEntityId   :: DocId
    , ownerEntityType :: EntityType
    , repositories    :: Maybe [UDEPR.EmProjectRepository]
    , updatedAt       :: UTCTime
    } deriving (Generic, Show)

data Build = Build
    { message :: Maybe T.Text
    , status  :: T.Text
    } deriving (Generic, Show)

-- Update the build status of the project according to the build status of its repositories.
updateBuild :: Project -> Project
updateBuild project = project { build = combineBuilds $ repositories project }

-- Calculate the combined build status of a list of repositories.
combineBuilds :: Maybe [UDEPR.EmProjectRepository] -> Maybe Build
combineBuilds maybeRepositories
    | length allRepositories == 0 = Nothing
    | nbFailingRepositories > 0   = Just failingBuild
    | nbWarningRepositories > 0   = Just warningBuild
    | nbUnknownRepositories > 0   = Nothing
    | otherwise                   = Just passingBuild

    where allRepositories       = maybe [] id maybeRepositories
          knownRepositories     = filter (\repository -> isJust $ UDEPR.headCommit repository) allRepositories
          nbUnknownRepositories = length $ filter (\repository -> not . isJust $ UDEPR.headCommit repository) allRepositories
          nbFailingRepositories = length $ filter (compareStatus "failing") knownRepositories
          nbWarningRepositories = length $ filter (compareStatus "warning") knownRepositories
          nbPassingRepositories = length $ filter (compareStatus "passing") knownRepositories

          compareStatus :: T.Text -> UDEPR.EmProjectRepository -> Bool
          compareStatus status repository = status == (UDEC.buildStatus $ fromJust (UDEPR.headCommit repository))


-- Functions/types for internal use.

instance FromJSON Project where
    parseJSON (Object v) =
           Project <$> v .:? "build"           .!= Nothing
                   <*> v .:  "createdAt"
                   <*> v .:  "isDeleted"
                   <*> v .:  "name"
                   <*> v .:  "ownerEntityId"
                   <*> v .:  "ownerEntityType"
                   <*> v .:? "repositories"    .!= Nothing
                   <*> v .:  "updatedAt"
    parseJSON invalid    = typeMismatch "Project" invalid

instance ToJSON Project where
    toJSON Project {..} = omitNulls
        [ "build"           .= build
        , "createdAt"       .= createdAt
        , "isDeleted"       .= isDeleted
        , "name"            .= name
        , "ownerEntityId"   .= ownerEntityId
        , "ownerEntityType" .= ownerEntityType
        , "repositories"    .= repositories
        , "updatedAt"       .= updatedAt
        ]

instance FromJSON Build where
    parseJSON (Object v) =
             Build <$> v .:? "message" .!= Nothing
                   <*> v .:  "status"
    parseJSON invalid    = typeMismatch "Build" invalid

instance ToJSON Build where
    toJSON Build {..} = omitNulls
        [ "message" .= message
        , "status"  .= status
        ]

passingBuild = Build
    { message = Nothing
    , status  = "passing"
    }

warningBuild = Build
    { message = Nothing
    , status  = "warning"
    }

failingBuild = Build
    { message = Nothing
    , status  = "failing"
    }
