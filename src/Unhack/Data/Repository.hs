{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Data.Repository
       ( emptyRepository
       , Repository(..)
       ) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T (Text)
import GHC.Generics (Generic)
import Unhack.Data.EmBranch (emptyEmBranch, EmBranch)


-- Public API.

data Repository = Repository
    { activeBranches :: [EmBranch]
    , defaultBranch  :: EmBranch
    , name           :: T.Text
    , picture        :: T.Text
    , previousUrls   :: [T.Text]
    , status         :: Bool
    , _type          :: T.Text
    , url            :: T.Text
    , vendor         :: T.Text
    , vendorId       :: T.Text
    } deriving (Generic, Show)

emptyRepository = Repository
    { activeBranches = []
    , defaultBranch  = emptyEmBranch
    , name           = ""
    , picture        = ""
    , previousUrls   = []
    , status         = True
    , _type          = ""
    , url            = ""
    , vendor         = ""
    , vendorId       = "" }


-- Functions/types for internal use.

instance FromJSON Repository where
    parseJSON (Object v) =
        Repository <$> v .:? "activeBranches" .!= []
                   <*> v .:? "defaultBranch"  .!= emptyEmBranch
                   <*> v .:  "name"
                   <*> v .:? "picture"        .!= ""
                   <*> v .:? "previousUrls"   .!= []
                   <*> v .:? "status"         .!= True
                   <*> v .:  "type"
                   <*> v .:  "url"
                   <*> v .:  "vendor"
                   <*> v .:  "vendorId"
    parseJSON invalid    = typeMismatch "Repository" invalid

instance ToJSON Repository where
    toJSON (Repository activeBranches defaultBranch name picture previousUrls status _type url vendor vendorId) =
        object [ "activeBranches" .= activeBranches
               , "defaultBranch"  .= defaultBranch
               , "name"           .= name
               , "picture"        .= picture
               , "previousUrls"   .= previousUrls
               , "status"         .= status
               , "type"           .= _type
               , "url"            .= url
               , "vendor"         .= vendor
               , "vendorId"       .= vendorId ]
