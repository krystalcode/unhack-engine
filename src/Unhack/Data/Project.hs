{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

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
import Database.Bloodhound.Types (DocId)
import Data.Maybe                (fromJust, isJust)
import GHC.Generics              (Generic)

import qualified Data.Text as T (Text)

-- Internal dependences.

import Unhack.Types (EntityType)

import qualified Unhack.Data.EmCommit            as UDEC  (EmCommit(..))
import qualified Unhack.Data.EmProjectRepository as UDEPR (EmProjectRepository(..))


-- Public API.

data Project = Project
    { build           :: Maybe Build
    , isDeleted       :: Bool
    , name            :: T.Text
    , ownerEntityType :: EntityType
    , ownerEntityId   :: DocId
    , repositories    :: Maybe [UDEPR.EmProjectRepository]
    } deriving (Generic, Show)

data Build = Build
    { status  :: T.Text
    , message :: Maybe T.Text
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
                   <*> v .:  "isDeleted"
                   <*> v .:  "name"
                   <*> v .:  "ownerEntityType"
                   <*> v .:  "ownerEntityId"
                   <*> v .:? "repositories"    .!= Nothing
    parseJSON invalid    = typeMismatch "Project" invalid

instance ToJSON Project where
    toJSON (Project build isDeleted name ownerEntityType ownerEntityId repositories) =
        object [ "build    "       .= build
               , "isDeleted"       .= isDeleted
               , "name"            .= name
               , "ownerEntityType" .= ownerEntityType
               , "ownerEntityId"   .= ownerEntityId
               , "repositories"    .= repositories ]

instance FromJSON Build where
    parseJSON (Object v) =
             Build <$> v .:  "status"
                   <*> v .:? "message" .!= Nothing
    parseJSON invalid    = typeMismatch "Build" invalid

instance ToJSON Build where
    toJSON (Build status message) =
        object [ "status"  .= status
               , "message" .= message ]

passingBuild = Build
    { status  = "passing"
    , message = Nothing   }

warningBuild = Build
    { status  = "warning"
    , message = Nothing   }

failingBuild = Build
    { status  = "failing"
    , message = Nothing   }
