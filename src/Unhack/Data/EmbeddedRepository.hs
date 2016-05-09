{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Data.EmbeddedRepository
       ( emptyEmbeddedRepository
       , EmbeddedRepository(..)
       ) where


-- Imports.

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GHC.Generics     (Generic)

import qualified Data.Text as T (Text)


-- Public API.

data EmbeddedRepository = EmbeddedRepository
    { _id :: T.Text
    , url :: T.Text } deriving (Generic, Show)

emptyEmbeddedRepository :: EmbeddedRepository
emptyEmbeddedRepository = EmbeddedRepository
    { _id = ""
    , url = "" }


-- Functions/types for internal use.

instance FromJSON EmbeddedRepository where
    parseJSON (Object v) = EmbeddedRepository <$>
                           v .: "id" <*>
                           v .: "url"
    parseJSON invalid    = typeMismatch "EmbeddedRepository" invalid

instance ToJSON EmbeddedRepository where
    toJSON (EmbeddedRepository _id url) =
        object [ "id" .= _id
               , "url" .= url ]
