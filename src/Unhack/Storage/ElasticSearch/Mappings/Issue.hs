{-# LANGUAGE OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Mappings.Issue
    ( issueMapping
    , IssueMapping(..)
    ) where


-- Imports.

import Data.Aeson
import Database.Bloodhound

import qualified Data.Text as T (Text)


-- Public API.

{-
    @Issue(
        "Check if we need to set the 'norms' and 'store' properties"
        type="improvement"
        priority="normal"
        labels="1.0.0-beta1, performance"
    )
-}
issueMapping = MappingName "issue"

data IssueMapping = IssueMapping deriving (Eq, Show)


-- Functions/types for internal use.

instance ToJSON IssueMapping where
    toJSON IssueMapping = object
        [ "issue" .= object
            [ "properties" .= object

                [ "commit" .= object
                    [ "type" .= ("nested" :: T.Text)
                    , "properties" .= object
                        [ "_id"  .= object
                            [ "type"   .= ("string"       :: T.Text)
                            , "index"  .= ("not_analyzed" :: T.Text) ]
                        , "hash" .= object
                            [ "type"   .= ("string"       :: T.Text)
                            , "index"  .= ("not_analyzed" :: T.Text) ]
                        , "time" .= object
                            [ "type"   .= ("date"         :: T.Text)
                            , "format" .= ("date_time"    :: T.Text) ]]] -- End of 'commit' field.

                , "createdAt" .= object
                    [ "type"   .= ("date"      :: T.Text)
                    , "format" .= ("date_time" :: T.Text) ] -- End of 'createdAt' field.

                , "file" .= object
                    [ "type"  .= ("string"       :: T.Text)
                    , "index" .= ("not_analyzed" :: T.Text) ] -- End of 'file' field.

                , "properties" .= object
                    [ "type" .= ("nested" :: T.Text)
                    , "properties" .= object
                        [ "labels" .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("not_analyzed" :: T.Text) ]
                        , "priority" .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("not_analyzed" :: T.Text) ]
                        , "title" .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("analyzed"     :: T.Text) ]
                        , "type" .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("not_analyzed" :: T.Text) ]]] -- End of 'properties' field.

                , "repository" .= object
                    [ "type" .= ("nested" :: T.Text)
                    , "properties" .= object
                        [ "_id" .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("not_analyzed" :: T.Text) ]
                        , "name" .= object
                            [ "type"  .= ("string"       :: T.Text)
                            , "index" .= ("not_analyzed" :: T.Text) ]]] -- End of 'repository' field.

                , "updatedAt" .= object
                    [ "type"   .= ("date"      :: T.Text)
                    , "format" .= ("date_time" :: T.Text) ] ]]] -- End of 'updatedAt' field.
