{-# LANGUAGE DeriveGeneric, RecordWildCards, OverloadedStrings #-}

module Unhack.Data.Repository
       ( Repository(..)
       ) where


-- Imports.

-- External dependencies.

import Data.Aeson
import Data.Aeson.Types          (typeMismatch)
import Database.Bloodhound.Types (DocId)
import Data.Time                 (UTCTime)
import GHC.Generics              (Generic)

import qualified Data.Text as T (Text)

-- Internal dependences.

import Unhack.Types (EntityType)
import Unhack.Util  (omitNulls)

import Unhack.Data.EmBranch (EmBranch)
import Unhack.Data.EmCommit (EmCommit)


-- Public API.

data Repository = Repository
    { activeBranches      :: Maybe [EmBranch]
    , createdAt           :: UTCTime
    , defaultBranch       :: Maybe EmBranch
    , headCommit          :: Maybe EmCommit
    , isAccessible        :: Bool
    , isActive            :: Bool
    , isDeleted           :: Bool
    , isPrivate           :: Bool
    , isProcessed         :: Bool
    , isQueuedToBeDeleted :: Maybe Bool
    , language            :: Maybe T.Text
    , name                :: T.Text
    , ownerEntityId       :: DocId
    , ownerEntityType     :: EntityType
    , picture             :: Maybe T.Text
    , previousUrls        :: Maybe [T.Text]
    {-
      @Issue(
        "Rename property from _type to type'"
        type="improvement"
        priority="low"
        labels="naming conventions"
      )
    -}
    , _type               :: T.Text
    , updatedAt           :: UTCTime
    , url                 :: T.Text
    , vendor              :: T.Text
    , vendorId            :: T.Text
    , vendorName          :: T.Text
    , vendorUsername      :: T.Text
    } deriving (Generic, Show)


-- Functions/types for internal use.

instance FromJSON Repository where
    parseJSON (Object v) =
        Repository <$> v .:? "activeBranches"      .!= Nothing
                   <*> v .:  "createdAt"
                   <*> v .:? "defaultBranch"       .!= Nothing
                   <*> v .:? "headCommit"          .!= Nothing
                   <*> v .:  "isAccessible"
                   <*> v .:  "isActive"
                   <*> v .:  "isDeleted"
                   <*> v .:  "isPrivate"
                   <*> v .:  "isProcessed"
                   <*> v .:? "isQueuedToBeDeleted" .!= Nothing
                   <*> v .:? "language"            .!= Nothing
                   <*> v .:  "name"
                   <*> v .:  "ownerEntityId"
                   <*> v .:  "ownerEntityType"
                   <*> v .:? "picture"             .!= Nothing
                   <*> v .:? "previousUrls"        .!= Nothing
                   <*> v .:  "type"
                   <*> v .:  "updatedAt"
                   <*> v .:  "url"
                   <*> v .:  "vendor"
                   <*> v .:  "vendorId"
                   <*> v .:  "vendorName"
                   <*> v .:  "vendorUsername"
    parseJSON invalid    = typeMismatch "Repository" invalid

instance ToJSON Repository where
    toJSON Repository {..} = omitNulls
        [ "activeBranches"      .= activeBranches
        , "createdAt"           .= createdAt
        , "defaultBranch"       .= defaultBranch
        , "headCommit"          .= headCommit
        , "isAccessible"        .= isAccessible
        , "isActive"            .= isActive
        , "isDeleted"           .= isDeleted
        , "isPrivate"           .= isPrivate
        , "isProcessed"         .= isProcessed
        , "isQueuedToBeDeleted" .= isQueuedToBeDeleted
        , "language"            .= language
        , "name"                .= name
        , "ownerEntityId"       .= ownerEntityId
        , "ownerEntityType"     .= ownerEntityType
        , "picture"             .= picture
        , "previousUrls"        .= previousUrls
        , "type"                .= _type
        , "updatedAt"           .= updatedAt
        , "url"                 .= url
        , "vendor"              .= vendor
        , "vendorId"            .= vendorId
        , "vendorName"          .= vendorName
        , "vendorUsername"      .= vendorUsername
        ]
