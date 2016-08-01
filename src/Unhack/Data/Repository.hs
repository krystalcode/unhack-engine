{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Data.Repository
       ( Repository(..)
       ) where


-- Imports.

-- External dependencies.

import Data.Aeson
import Data.Aeson.Types          (typeMismatch)
import Database.Bloodhound.Types (DocId)
import GHC.Generics              (Generic)

import qualified Data.Text as T (Text)

-- Internal dependences.

import Unhack.Types (EntityType)

import Unhack.Data.EmBranch (emptyEmBranch, EmBranch)
import Unhack.Data.EmCommit (emptyEmCommit, EmCommit)


-- Public API.

{-
  @Issue(
    "Consider using Maybe in non-mandatory properties"
    type="bug"
    priority="normal"
  )
-}
data Repository = Repository
    { activeBranches      :: [EmBranch]
    , defaultBranch       :: Maybe EmBranch
    , headCommit          :: Maybe EmCommit
    , isAccessible        :: Bool
    , isActive            :: Bool
    , isDeleted           :: Bool
    , isProcessed         :: Bool
    , isPrivate           :: Bool
    , isQueuedToBeDeleted :: Maybe Bool
    , language            :: Maybe T.Text
    , name                :: T.Text
    , ownerEntityType     :: EntityType
    , ownerEntityId       :: DocId
    , picture             :: Maybe T.Text
    , previousUrls        :: Maybe [T.Text]
    , _type               :: T.Text
    , url                 :: T.Text
    , vendor              :: T.Text
    , vendorId            :: T.Text
    , vendorName          :: T.Text
    , vendorUsername      :: T.Text
    } deriving (Generic, Show)


-- Functions/types for internal use.

instance FromJSON Repository where
    parseJSON (Object v) =
        Repository <$> v .:? "activeBranches"      .!= []
                   <*> v .:? "defaultBranch"       .!= Nothing
                   <*> v .:? "headCommit"          .!= Nothing
                   <*> v .:  "isAccessible"
                   <*> v .:  "isActive"
                   <*> v .:  "isDeleted"
                   <*> v .:  "isProcessed"
                   <*> v .:  "isPrivate"
                   <*> v .:  "isQueuedToBeDeleted" .!= Nothing
                   <*> v .:  "language"            .!= Nothing
                   <*> v .:  "name"
                   <*> v .:  "ownerEntityType"
                   <*> v .:  "ownerEntityId"
                   <*> v .:? "picture"             .!= Nothing
                   <*> v .:? "previousUrls"        .!= Nothing
                   <*> v .:  "type"
                   <*> v .:  "url"
                   <*> v .:  "vendor"
                   <*> v .:  "vendorId"
                   <*> v .:  "vendorName"
                   <*> v .:  "vendorUsername"
    parseJSON invalid    = typeMismatch "Repository" invalid

instance ToJSON Repository where
    toJSON (Repository activeBranches
                       defaultBranch
                       headCommit
                       isAccessible
                       isActive
                       isDeleted
                       isProcessed
                       isPrivate
                       isQueuedToBeDeleted
                       language
                       name
                       ownerEntityType
                       ownerEntityId
                       picture
                       previousUrls
                       _type
                       url
                       vendor
                       vendorId
                       vendorName
                       vendorUsername
           ) =
        object [ "activeBranches"      .= activeBranches
               , "defaultBranch"       .= defaultBranch
               , "headCommit"          .= headCommit
               , "isAccessible"        .= isAccessible
               , "isActive"            .= isActive
               , "isDeleted"           .= isDeleted
               , "isProcessed"         .= isProcessed
               , "isPrivate"           .= isPrivate
               , "isQueuedToBeDeleted" .= isQueuedToBeDeleted
               , "language"            .= language
               , "name"                .= name
               , "ownerEntityType"     .= ownerEntityType
               , "ownerEntityId"       .= ownerEntityId
               , "picture"             .= picture
               , "previousUrls"        .= previousUrls
               , "type"                .= _type
               , "url"                 .= url
               , "vendor"              .= vendor
               , "vendorId"            .= vendorId
               , "vendorName"          .= vendorName
               , "vendorUsername"      .= vendorUsername ]
