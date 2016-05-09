{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Data.EmBranch
       ( emptyEmBranch
       , EmBranch(..)
       ) where


-- Imports.

-- External dependencies.

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GHC.Generics     (Generic)

-- Internal dependencies.

import qualified Data.Text as T (Text)


-- Public API.

data EmBranch = EmBranch
    { _id  :: T.Text
    , name :: T.Text } deriving (Generic, Show)

emptyEmBranch :: EmBranch
emptyEmBranch = EmBranch
    { _id  = ""
    , name = "" }


-- Functions/types for internal use.

instance FromJSON EmBranch where
    parseJSON (Object v) = EmBranch
                           <$> v .: "_id"
                           <*> v .: "name"
    parseJSON invalid    = typeMismatch "EmBranch" invalid

instance ToJSON EmBranch where
    toJSON (EmBranch _id name) =
        object [ "_id"  .= _id
               , "name" .= name ]
