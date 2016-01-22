{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Storage.Config
       ( load
       , StorageConfig(..)
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
import GHC.Generics -- (Generic)

-- Public API.

load :: FilePath -> IO (StorageConfig)
load filepath = do
    ymlData <- BS.readFile filepath
    let config = Y.decode ymlData :: Maybe StorageConfig
    return $ fromJust config

data StorageConfig = StorageConfig
    { type' :: T.Text
    , host :: T.Text
    , port :: Int
    } deriving (Generic, Show)


-- Functions/types for internal use.

instance FromJSON StorageConfig where
    parseJSON (Object v) = StorageConfig <$>
                           v .: "type" <*>
                           v .: "host" <*>
                           v .: "port"
    parseJSON invalid    = typeMismatch "StorageConfig" invalid

instance ToJSON StorageConfig where
    toJSON (StorageConfig type' host port) =
        object [ "type" .= type'
               , "host" .= host
               , "port" .= port ]
