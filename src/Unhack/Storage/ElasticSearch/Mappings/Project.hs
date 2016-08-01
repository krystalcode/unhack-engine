{-# LANGUAGE OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Mappings.Project
       ( projectMapping
       , ProjectMapping(..) )
       where


-- Imports.

import Data.Aeson
import Database.Bloodhound

import qualified Data.Text as T (concat, pack, Text)


-- Public API.

projectMapping = MappingName "project"

data ProjectMapping = ProjectMapping deriving (Eq, Show)


-- Functions/types for internal use.

instance ToJSON ProjectMapping where
    toJSON ProjectMapping = object
        [ "project" .= object
            [ "properties" .= object

                [ "build" .= object
                    [ "type"  .= ("nested" :: T.Text)
                    , "properties" .= object
                        [ "status"  .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("not_analyzed" :: T.Text) ]
                        , "message" .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("not_analyzed" :: T.Text) ]]] -- End of 'build' field.

                , "isDeleted" .= object
                    [ "type"  .= ("boolean"      :: T.Text)
                    , "index" .= ("not_analyzed" :: T.Text) ] -- End of 'isDeleted' field.

                , "name" .= object
                    [ "type"  .= ("string"       :: T.Text)
                    , "index" .= ("not_analyzed" :: T.Text) ] -- End of 'name' field.

                , "ownerEntityType" .= object
                    [ "type"  .= ("string"       :: T.Text)
                    , "index" .= ("not_analyzed" :: T.Text) ] -- End of 'ownerEntityType' field.

                , "ownerEntityId" .= object
                    [ "type"  .= ("string"       :: T.Text)
                    , "index" .= ("not_analyzed" :: T.Text) ] -- End of 'ownerEntityId' field.

                , "repositories" .= object
                    [ "type"  .= ("nested" :: T.Text)
                    , "properties" .= object

                        [ "_id"  .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("not_analyzed" :: T.Text) ] -- End of 'repositories._id' field.

                        , "name" .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("not_analyzed" :: T.Text) ] -- End of 'repositories.name' field.

                        , "defaultBranch"   .= object
                            [ "type"  .= ("nested"         :: T.Text)
                            , "properties" .= object
                                [ "_id" .= object
                                    [ "type"   .= ("string"       :: T.Text)
                                    , "index"  .= ("not_analyzed" :: T.Text) ]
                                , "name" .= object
                                    [ "type"   .= ("string"       :: T.Text)
                                    , "index"  .= ("not_analyzed" :: T.Text) ]]] -- End of 'repositories.defaultBranch' field.

                        , "headCommit" .= object
                            [ "type"  .= ("nested"         :: T.Text)
                            , "properties" .= object
                                [ "_id"          .= object
                                    [ "type"   .= ("string"       :: T.Text)
                                    , "index"  .= ("not_analyzed" :: T.Text) ]
                                , "hash" .= object
                                    [ "type"   .= ("string"       :: T.Text)
                                    , "index"  .= ("not_analyzed" :: T.Text) ]
                                , "time" .= object
                                    [ "type"   .= ("date"         :: T.Text)
                                    , "format" .= ("epoch_second" :: T.Text) ]
                                , "buildStatus" .= object
                                    [ "type"   .= ("string"       :: T.Text)
                                    , "index"  .= ("not_analyzed" :: T.Text) ]
                                , "buildMessage" .= object
                                    [ "type"   .= ("string"       :: T.Text)
                                    , "index"  .= ("not_analyzed" :: T.Text) ]]] -- End of 'repositories.headCommit' field.

                        ]] -- End of 'repositories' field.

                ]]]
