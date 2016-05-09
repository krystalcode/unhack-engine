{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Config
       ( load
       , indexName
       , indexSettingsFromConfig
       , StorageConfig(..)
       , StorageIndexSettings(..)
       ) where

{-
  @Issue(
    "Find out why restricting Data.Aeson imports results in error not
    recognising Object"
    type="task"
    priority="low"
  )
-}
import Data.Aeson -- ((.=), (.:), parseJSON, toJSON, FromJSON, ToJSON)
import Data.Aeson.Types (typeMismatch) --, object, Object)
import qualified Data.ByteString.Char8 as BS (readFile)
import Data.Maybe (fromJust)
import qualified Data.Text as T (Text)
import qualified Data.Yaml as Y (decode)
import qualified Database.Bloodhound as BH
import GHC.Generics (Generic)

-- Public API.

-- Load storage configuration from a file.
load :: FilePath -> IO (StorageConfig)
load filepath = do
    ymlData <- BS.readFile filepath
    let config = Y.decode ymlData :: Maybe StorageConfig
    return $ fromJust config

-- Get the name of an index from its settings.
indexName :: StorageIndexSettings -> BH.IndexName
indexName settings = BH.IndexName $ name settings

-- Get the settings for a specific index from the storage configuration.
indexSettingsFromConfig :: T.Text -> StorageConfig -> StorageIndexSettings
indexSettingsFromConfig indexKey config = matchAsList !! 0
    where allIndexes  = indexes config
          matchAsList = filter (\x@(StorageIndexSettings key _ _ _) -> key == indexKey) allIndexes

{-
  @Issue(
    "Remove the 'type' option and provide default for other options making them
    optional"
    type="improvement"
    priority="low"
  )
-}
data StorageConfig = StorageConfig
    { type' :: T.Text
    , host :: T.Text
    , port :: Int
    , indexes :: [StorageIndexSettings]
    } deriving (Generic, Show)

data StorageIndexSettings = StorageIndexSettings
    { key :: T.Text
    , name :: T.Text
    , shards :: Int
    , replicas :: Int
    } deriving (Generic, Show)

-- Functions/types for internal use.

instance FromJSON StorageConfig where
    parseJSON (Object v) = StorageConfig <$>
                           v .: "type" <*>
                           v .: "host" <*>
                           v .: "port" <*>
                           v .: "indexes"
    parseJSON invalid    = typeMismatch "StorageConfig" invalid

instance ToJSON StorageConfig where
    toJSON (StorageConfig type' host port indexes) =
        object [ "type" .= type'
               , "host" .= host
               , "port" .= port
               , "indexes" .= indexes ]

instance FromJSON StorageIndexSettings where
    parseJSON (Object v) = StorageIndexSettings <$>
                           v .: "key" <*>
                           v .: "name" <*>
                           v .: "shards" <*>
                           v .: "replicas"
    parseJSON invalid    = typeMismatch "StorageIndexSettings" invalid

instance ToJSON StorageIndexSettings where
    toJSON (StorageIndexSettings key name shards replicas) =
        object [ "name" .= name
               , "shards" .= shards
               , "replicas" .= replicas ]
