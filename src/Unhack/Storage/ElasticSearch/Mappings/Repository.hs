{-# LANGUAGE OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Mappings.Repository
    ( repositoryMapping
    , RepositoryMapping(..)
    ) where


-- Imports.

import Data.Aeson
import Database.Bloodhound

import qualified Data.Text as T (Text)


-- Public API.

repositoryMapping = MappingName "repository"

data RepositoryMapping = RepositoryMapping deriving (Eq, Show)


-- Functions/types for internal use.

instance ToJSON RepositoryMapping where
    toJSON RepositoryMapping = object
        [ "repository" .= object
            [ "properties" .= object

                [ "activeBranches" .= object
                    [ "type" .= ("nested" :: T.Text)
                    , "properties" .= object
                        [ "_id" .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("not_analyzed" :: T.Text) ]
                        , "name" .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("not_analyzed" :: T.Text) ]]] -- End of 'activeBranches' field.

                , "createdAt" .= object
                    [ "type"   .= ("date"      :: T.Text)
                    , "format" .= ("date_time" :: T.Text) ] -- End of 'createdAt' field.

                , "defaultBranch" .= object
                    [ "type" .= ("nested" :: T.Text)
                    , "properties" .= object
                        [ "_id" .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("not_analyzed" :: T.Text) ]
                        , "name" .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("not_analyzed" :: T.Text) ]]] -- End of 'defaultBranch' field.

                , "headCommit" .= object
                    [ "type"  .= ("nested" :: T.Text)
                    , "properties" .= object
                        [ "_id" .= object
                            [ "type"   .= ("string"       :: T.Text)
                            , "index"  .= ("not_analyzed" :: T.Text) ]
                        , "buildMessage" .= object
                            [ "type"   .= ("string"       :: T.Text)
                            , "index"  .= ("not_analyzed" :: T.Text) ]
                        , "buildStatus" .= object
                            [ "type"   .= ("string"       :: T.Text)
                            , "index"  .= ("not_analyzed" :: T.Text) ]
                        , "hash" .= object
                            [ "type"   .= ("string"       :: T.Text)
                            , "index"  .= ("not_analyzed" :: T.Text) ]
                        , "time" .= object
                            [ "type"   .= ("date"         :: T.Text)
                            , "format" .= ("date_time"    :: T.Text) ]]] -- End of 'headCommit' field.

                , "isAccessible" .= object
                    [ "type"   .= ("boolean"      :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'isAccessible' field.

                , "isActive" .= object
                    [ "type"   .= ("boolean"      :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'isActive' field.

                , "isDeleted" .= object
                    [ "type"   .= ("boolean"      :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'isDeleted' field.

                , "isPrivate" .= object
                    [ "type"   .= ("boolean"      :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'isPrivate' field.

                , "isProcessed" .= object
                    [ "type"   .= ("boolean"      :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'isProcessed' field.

                , "isQueuedToBeDeleted" .= object
                    [ "type"   .= ("boolean"      :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'isQueuedToBeDeleted' field.

                , "language" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'language' field.

                , "name" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'name' field.

                , "ownerEntityId" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'ownerEntityId' field.

                , "ownerEntityType" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'ownerEntityType field.

                , "picture" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'picture' field.

                , "previousUrls" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'previousUrls' field.

                , "type" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'type' field.

                , "updatedAt" .= object
                    [ "type"   .= ("date"         :: T.Text)
                    , "format" .= ("date_time"    :: T.Text) ] -- End of 'updatedAt' field.

                , "url" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'url' field.

                , "vendor" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'vendor' field.

                , "vendorId" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'vendorId' field.

                , "vendorName" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'vendorName' field.

                , "vendorUsername" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] ]]] -- End of 'vendorUsername' field.
