{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Data.Branch
       ( emptyBranch
       , bulkSetRepositoryId
       , Branch(..)
       ) where


-- Imports.

-- External dependencies.

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GHC.Generics     (Generic)

import qualified Data.Text as T (Text)

-- Internal dependencies.

import Unhack.Data.EmCommit (emptyEmCommit, EmCommit)


-- Public API.

data Branch = Branch { headCommit   :: Maybe EmCommit
                     , isActive     :: Bool
                     , name         :: T.Text
                     , repositoryId :: T.Text
                     } deriving (Generic, Show)

emptyBranch = Branch { headCommit   = Nothing
                     , isActive     = False
                     , name         = ""
                     , repositoryId = "" }

bulkSetRepositoryId :: [Branch] -> T.Text -> [Branch]
bulkSetRepositoryId branches repositoryId = map (\branch -> branch { repositoryId = repositoryId }) branches


-- Functions/types for internal use.

instance FromJSON Branch where
    parseJSON (Object v) = Branch
                           <$> v .:? "headCommit" .!= Nothing
                           <*> v .:  "isActive"
                           <*> v .:  "name"
                           <*> v .:  "repositoryId"
    parseJSON invalid    = typeMismatch "Branch" invalid

instance ToJSON Branch where
    toJSON (Branch headCommit isActive name repositoryId) =
        object [ "headCommit"   .= headCommit
               , "isActive"     .= isActive
               , "name"         .= name
               , "repositoryId" .= repositoryId ]
