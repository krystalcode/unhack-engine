{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Data.EmProjectRepository
       ( fromRepository
       , EmProjectRepository(..)
       ) where


-- Imports.

-- External dependencies.

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GHC.Generics     (Generic)

import qualified Data.Text as T (Text)

-- Internal dependencies.

import qualified Unhack.Data.EmBranch   as UDEB (EmBranch)
import qualified Unhack.Data.EmCommit   as UDEC (EmCommit)
import qualified Unhack.Data.Repository as UDR  (Repository(..))


-- Public API.

data EmProjectRepository = EmProjectRepository
    { _id           :: T.Text
    , name          :: T.Text
    , defaultBranch :: Maybe UDEB.EmBranch
    , headCommit    :: Maybe UDEC.EmCommit } deriving (Generic, Show)

-- Convert a Repository record to an EmProjectRepository record.
fromRepository :: T.Text -> UDR.Repository -> EmProjectRepository
fromRepository repositoryId repository = EmProjectRepository
    { _id           = repositoryId
    , name          = UDR.name repository
    , defaultBranch = UDR.defaultBranch repository
    , headCommit    = UDR.headCommit repository    }


-- Functions/types for internal use.

instance FromJSON EmProjectRepository where
    parseJSON (Object v) =
        EmProjectRepository <$> v .:  "_id"
                            <*> v .:  "name"
                            <*> v .:? "defaultBranch" .!= Nothing
                            <*> v .:? "headCommit"    .!= Nothing
    parseJSON invalid    = typeMismatch "EmProjectRepository" invalid

instance ToJSON EmProjectRepository where
    toJSON (EmProjectRepository _id name defaultBranch headCommit) =
        object [ "_id"           .= _id
               , "name"          .= name
               , "defaultBranch" .= defaultBranch
               , "headCommit"    .= headCommit    ]
