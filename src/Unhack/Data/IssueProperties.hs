{-# LANGUAGE DeriveGeneric, RecordWildCards, OverloadedStrings #-}

module Unhack.Data.IssueProperties
    ( accessMultiValueProperty
    , accessSingleValueProperty
    , IssueProperties(..)
    ) where


-- Imports.

-- External dependencies.

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GHC.Generics     (Generic)

import qualified Data.Text as T (Text)

-- Internal dependencies.

import Unhack.Util (omitNulls)


-- Public API.

data IssueProperties = IssueProperties
    { labels   :: Maybe [T.Text]
    , notes    :: Maybe T.Text
    , priority :: Maybe T.Text
    , title    :: T.Text
    , type'    :: Maybe T.Text
    } deriving (Generic, Show)

-- Given the name of a multi-value property e.g. a property that is a list of
-- texts, get value of the property.
accessMultiValueProperty :: IssueProperties -> T.Text -> Maybe [T.Text]
accessMultiValueProperty issueProperties "labels" = labels issueProperties

-- Given the name of a single-value property e.g. a property that is a text, get
-- the value of the property.
{-
  @Issue(
    "There must be a better way than pattern matching",
    type="improvement"
    priority="low"
  )
-}
accessSingleValueProperty :: IssueProperties -> T.Text -> Maybe T.Text
accessSingleValueProperty issueProperties "priority" = priority issueProperties
accessSingleValueProperty issueProperties "title"    = Just (title issueProperties)
accessSingleValueProperty issueProperties "type"     = type' issueProperties


-- Functions/types for internal use.

-- JSON conversions.
instance FromJSON IssueProperties where
    parseJSON (Object v) =
        IssueProperties <$> v .:? "notes"    .!= Nothing
                        <*> v .:? "labels"   .!= Nothing
                        <*> v .:? "priority" .!= Nothing
                        <*> v .:  "title"
                        <*> v .:? "type"     .!= Nothing
    parseJSON invalid    = typeMismatch "IssueProperties" invalid

instance ToJSON IssueProperties where
    toJSON IssueProperties {..} = omitNulls
        [ "notes"    .= notes
        , "labels"   .= labels
        , "priority" .= priority
        , "title"    .= title
        , "type"     .= type'
        ]
