{-# LANGUAGE OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Mappings.Branch
    ( branchMapping
    , BranchMapping(..)
    ) where


-- Imports.

import Data.Aeson
import Database.Bloodhound

import qualified Data.Text as T (Text)


-- Public API.

branchMapping = MappingName "branch"

data BranchMapping = BranchMapping deriving (Eq, Show)


-- Functions/types for internal use.

instance ToJSON BranchMapping where
    toJSON BranchMapping = object
        [ "branch" .= object
            [ "properties" .= object

                [ "createdAt" .= object
                    [ "type"   .= ("date"        :: T.Text)
                    , "format" .= ("date_time"   :: T.Text) ] -- End of 'createdAt' field.

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

                , "isActive" .= object
                    [ "type"  .= ("boolean"      :: T.Text)
                    , "index" .= ("not_analyzed" :: T.Text) ] -- End of 'isActive' field.

                , "name" .= object
                    [ "type"  .= ("string"       :: T.Text)
                    , "index" .= ("not_analyzed" :: T.Text) ] -- End of 'name' field.

                , "repositoryId" .= object
                    [ "type"  .= ("string"       :: T.Text)
                    , "index" .= ("not_analyzed" :: T.Text) ] -- End of 'repositoryId' field.

                , "updatedAt" .= object
                    [ "type"   .= ("date"        :: T.Text)
                    , "format" .= ("date_time"   :: T.Text) ] ]]] -- End of 'updatedAt' field.
