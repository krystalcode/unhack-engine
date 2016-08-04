{-# LANGUAGE DeriveGeneric, RecordWildCards, OverloadedStrings #-}

module Unhack.Data.Branch
       ( bulkSetRepositoryId
       , Branch(..)
       ) where


-- Imports.

-- External dependencies.

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Time        (UTCTime)
import GHC.Generics     (Generic)

import qualified Data.Text as T (Text)

-- Internal dependencies.

import Unhack.Data.EmCommit (EmCommit)
import Unhack.Util          (omitNulls)


-- Public API.

data Branch = Branch { createdAt    :: UTCTime
                     , headCommit   :: Maybe EmCommit
                     , isActive     :: Bool
                     , name         :: T.Text
                     , repositoryId :: T.Text
                     , updatedAt    :: UTCTime
                     } deriving (Generic, Show)

bulkSetRepositoryId :: [Branch] -> T.Text -> [Branch]
bulkSetRepositoryId branches repositoryId = map (\branch -> branch { repositoryId = repositoryId }) branches


-- Functions/types for internal use.

instance FromJSON Branch where
    parseJSON (Object v) = Branch
                           <$> v .:  "createdAt"
                           <*> v .:? "headCommit" .!= Nothing
                           <*> v .:  "isActive"
                           <*> v .:  "name"
                           <*> v .:  "repositoryId"
                           <*> v .:  "updatedAt"
    parseJSON invalid    = typeMismatch "Branch" invalid

instance ToJSON Branch where
    toJSON Branch {..} = omitNulls
        [ "createdAt"    .= createdAt
        , "headCommit"   .= headCommit
        , "isActive"     .= isActive
        , "name"         .= name
        , "repositoryId" .= repositoryId
        , "updatedAt"    .= updatedAt
        ]
