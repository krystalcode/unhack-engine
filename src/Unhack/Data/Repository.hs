{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Data.Repository
       ( emptyRepository
       , Repository(..)
       ) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T (Text)
import GHC.Generics (Generic)


-- Public API.

data Repository = Repository
    { url :: T.Text
    , previousUrls :: [T.Text]
    , project :: T.Text
    } deriving (Generic, Show)

emptyRepository = Repository
    { url = ""
    , previousUrls = []
    , project = "" }


-- Functions/types for internal use.

instance FromJSON Repository where
    parseJSON (Object v) = Repository <$>
                           v .: "url" <*>
                           v .: "previousUrls" <*>
                           v .: "project"
    parseJSON invalid    = typeMismatch "Repository" invalid

instance ToJSON Repository where
    toJSON (Repository url previousUrls project) =
        object [ "url" .= url
               , "previous_urls" .= previousUrls
               , "project" .= project ]
