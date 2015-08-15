module Unhack.Project
       ( Project(..)
       ) where

import Data.Aeson
import GHC.Generics
import qualified Unhack.ElasticSearch as ES

projectMapping = MappingName "project"
indexIssue = withBH' $ indexDocument ES.index projectMapping defaultIndexDocumentSettings

data Project = Project { userId :: Int
                       , vendor :: String
                       , repo :: String
                       } deriving (Generic, Show)

instance ToJSON Project
