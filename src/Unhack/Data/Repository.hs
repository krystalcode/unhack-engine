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
import Unhack.Data.EmCommit (emptyEmCommit, EmCommit)


-- Public API.

{-
  @Issue(
    "Consider using Maybe in non-mandatory properties"
    type="bug"
    priority="normal"
  )
-}
data Repository = Repository
    { activeBranches :: [EmBranch]
    , defaultBranch  :: EmBranch
    , headCommit     :: Maybe EmCommit
    , isAccessible   :: Bool
    , isActive       :: Bool
    , isDeleted      :: Bool
    , isProcessed    :: Bool
    , isPrivate      :: Bool
    , name           :: T.Text
    , picture        :: T.Text
    , previousUrls   :: [T.Text]
    , _type          :: T.Text
    , url            :: T.Text
    , vendor         :: T.Text
    , vendorId       :: T.Text
    , vendorName     :: T.Text
    , vendorUsername :: T.Text
    } deriving (Generic, Show)

emptyRepository = Repository
    { activeBranches = []
    , defaultBranch  = emptyEmBranch
    , headCommit     = Nothing
    , isAccessible   = False
    , isActive       = True
    , isDeleted      = False
    , isProcessed    = False
    , isPrivate      = True
    , name           = ""
    , picture        = ""
    , previousUrls   = []
    , _type          = ""
    , url            = ""
    , vendor         = ""
    , vendorId       = ""
    , vendorName     = ""
    , vendorUsername = "" }


-- Functions/types for internal use.

instance FromJSON Repository where
    parseJSON (Object v) =
        Repository <$> v .:? "activeBranches" .!= []
                   <*> v .:? "defaultBranch"  .!= emptyEmBranch
                   <*> v .:? "headCommit"     .!= Nothing
                   <*> v .:  "isAccessible"
                   <*> v .:  "isActive"
                   <*> v .:  "isDeleted"
                   <*> v .:  "isProcessed"
                   <*> v .:  "isPrivate"
                   <*> v .:  "name"
                   <*> v .:? "picture"        .!= ""
                   <*> v .:? "previousUrls"   .!= []
                   <*> v .:  "type"
                   <*> v .:  "url"
                   <*> v .:  "vendor"
                   <*> v .:  "vendorId"
                   <*> v .:  "vendorName"
                   <*> v .:  "vendorUsername"
    parseJSON invalid    = typeMismatch "Repository" invalid

instance ToJSON Repository where
    toJSON (Repository activeBranches defaultBranch headCommit isAccessible isActive isDeleted isProcessed isPrivate name picture previousUrls _type url vendor vendorId vendorName vendorUsername) =
        object [ "activeBranches" .= activeBranches
               , "defaultBranch"  .= defaultBranch
               , "headCommit"     .= headCommit
               , "isAccessible"   .= isAccessible
               , "isActive"       .= isActive
               , "isDeleted"      .= isDeleted
               , "isProcessed"    .= isProcessed
               , "isPrivate"      .= isPrivate
               , "name"           .= name
               , "picture"        .= picture
               , "previousUrls"   .= previousUrls
               , "type"           .= _type
               , "url"            .= url
               , "vendor"         .= vendor
               , "vendorId"       .= vendorId
               , "vendorName"     .= vendorName
               , "vendorUsername" .= vendorUsername ]
