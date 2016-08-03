{-# LANGUAGE OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Mappings.Commit
    ( commitMapping
    , CommitMapping(..)
    ) where


-- Imports.

import Data.Aeson
import Database.Bloodhound

import qualified Data.Text as T (Text)


-- Public API.

commitMapping = MappingName "commit"

data CommitMapping = CommitMapping deriving (Eq, Show)


-- Functions/types for internal use.

instance ToJSON CommitMapping where
    toJSON CommitMapping = object
        [ "commit" .= object
            [ "properties" .= object

                [ "branches" .= object
                    [ "type" .= ("nested" :: T.Text)
                    , "properties" .= object
                        [ "_id" .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("not_analyzed" :: T.Text) ]
                        , "name" .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("not_analyzed" :: T.Text) ]]] -- End of 'branches' field.

                , "buildMessage" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'buildMessage' field.

                , "buildStatus" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'buildStatus' field.

                , "createdAt" .= object
                    [ "type"   .= ("date"         :: T.Text)
                    , "format" .= ("date_time"    :: T.Text) ] -- End of 'createdAt' field.

                , "hash" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'hash' field.

                , "isProcessed" .= object
                    [ "type"   .= ("boolean"      :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'isProcessed' field.

                , "repositoryId" .= object
                    [ "type"   .= ("string"       :: T.Text)
                    , "index"  .= ("not_analyzed" :: T.Text) ] -- End of 'repositoryId' field.

                , "time" .= object
                    [ "type"   .= ("date"         :: T.Text)
                    , "format" .= ("date_time"    :: T.Text) ] -- End of 'time' field.

                , "updatedAt" .= object
                    [ "type"   .= ("date"         :: T.Text)
                    , "format" .= ("date_time"    :: T.Text) ] ]]] -- End of 'updatedAt' field.
