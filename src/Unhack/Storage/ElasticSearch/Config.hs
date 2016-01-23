{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Storage.ElasticSearch.Config
       ( load
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
import GHC.Generics (Generic)

-- Public API.

load :: FilePath -> IO (StorageConfig)
load filepath = do
    ymlData <- BS.readFile filepath
    let config = Y.decode ymlData :: Maybe StorageConfig
    return $ fromJust config


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
    , index :: StorageIndexSettings
    } deriving (Generic, Show)

data StorageIndexSettings = StorageIndexSettings
    { name :: T.Text
    , shards :: Int
    , replicas :: Int
    } deriving (Generic, Show)

-- Functions/types for internal use.

instance FromJSON StorageConfig where
    parseJSON (Object v) = StorageConfig <$>
                           v .: "type" <*>
                           v .: "host" <*>
                           v .: "port" <*>
                           v .: "index"
    parseJSON invalid    = typeMismatch "StorageConfig" invalid

instance ToJSON StorageConfig where
    toJSON (StorageConfig type' host port index) =
        object [ "type" .= type'
               , "host" .= host
               , "port" .= port
               , "index" .= index ]

instance FromJSON StorageIndexSettings where
    parseJSON (Object v) = StorageIndexSettings <$>
                           v .: "name" <*>
                           v .: "shards" <*>
                           v .: "replicas"
    parseJSON invalid    = typeMismatch "StorageIndexSettings" invalid

instance ToJSON StorageIndexSettings where
    toJSON (StorageIndexSettings name  shards replicas) =
        object [ "name" .= name
               , "shards" .= shards
               , "replicas" .= replicas ]
