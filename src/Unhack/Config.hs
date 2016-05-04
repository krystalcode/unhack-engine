{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Config
       ( load
       , Config(..)
       ) where


-- Imports.

import Control.Exception (catch, throwIO)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Char8 as BS (readFile)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T (Text);
import qualified Data.Yaml as Y (decode)
import GHC.Generics (Generic)
import System.IO.Error (isDoesNotExistError)


-- Public API.

-- Load storage configuration from a file. If the file does not exist, return
-- the default configuration.
load :: FilePath -> IO (Config)
load filepath = do
    ymlData <- BS.readFile filepath `catch` handleExists
    let config = Y.decode ymlData :: Maybe Config
    if (isNothing config)
        then return confDefault
        else return $ fromJust config

    where handleExists e
              | isDoesNotExistError e = return ""
              | otherwise             = throwIO e

-- Types.

data Config = Config
    { confAnnotations :: [Annotation] } deriving (Generic, Show)

data Annotation = Annotation
    { annAnalysis :: Analysis
    , annBuild    :: Build } deriving (Generic, Show)

data Analysis = Analysis
    { anaName         :: T.Text
    , anaProperties   :: [AnalysisProperty]
    , anaFilePatterns :: [T.Text] } deriving (Generic, Show)

data AnalysisProperty = AnalysisProperty
    { apName   :: T.Text
    , apType   :: T.Text
    , apValue  :: [T.Text]
    , apPolicy :: T.Text } deriving (Generic, Show)

data Build = Build
    { bRules :: [Rule] } deriving (Generic, Show)

data Rule = Rule
    { rOperator   :: T.Text
    , rConditions :: [Condition]
    , rActions    :: [Action] } deriving (Generic, Show)

data Condition = Condition
    { condType       :: T.Text
    , condCount      :: Int
    , condOperator   :: T.Text
    , condProperties :: [ConditionProperty] } deriving (Generic, Show)

data ConditionProperty = ConditionProperty
    { cpName  :: T.Text
    , cpValue :: T.Text } deriving (Generic, Show)

data Action = Action
    { acType :: T.Text
    , acData :: ActionData } deriving (Generic, Show)

{-
  @Issue(
    "Add action types for 'email', 'webhook', 'jira' and other CI services"
    type="improvement"
    priority="normal"
  )
-}
data ActionData = ActionDataStatus
    { adsName :: T.Text } deriving (Generic, Show)


-- Functions/types for internal use.

-- Default Config.
confDefault = Config
    { confAnnotations = [ annDefault ] }

-- From/To JSON definitions for Config.
instance FromJSON Config where
    parseJSON (Object v) = Config
                           <$> v .:? "annotations" .!= [annDefault]
    parseJSON invalid    = typeMismatch "Config" invalid

instance ToJSON Config where
    toJSON (Config confAnnotations) =
        object [ "annotations" .= confAnnotations ]

-- Default Annotation.
annDefault = Annotation
    { annAnalysis = anaDefault
    , annBuild    = bDefault }

-- From/To JSON definitions for Annotation.
instance FromJSON Annotation where
    parseJSON (Object v) = Annotation
                           <$> v .:? "analysis" .!= anaDefault
                           <*> v .:? "build"    .!= bDefault
    parseJSON invalid    = typeMismatch "Annotation" invalid

instance ToJSON Annotation where
    toJSON (Annotation annAnalysis annBuild) =
        object [ "analysis" .= annAnalysis
               , "build"    .= annBuild ]

-- Default Analysis.
anaDefault = Analysis
    { anaName         = anaDefaultName
    , anaProperties   = anaDefaultProperties
    , anaFilePatterns = anaDefaultFilePatterns }

-- From/To JSON definitions for Analysis.
instance FromJSON Analysis where
    parseJSON (Object v) = Analysis
                           <$> v .:? "name"          .!= anaDefaultName
                           <*> v .:? "properties"    .!= anaDefaultProperties
                           <*> v .:? "file_patterns" .!= anaDefaultFilePatterns
    parseJSON invalid    = typeMismatch "Analysis" invalid

instance ToJSON Analysis where
    toJSON (Analysis anaName anaProperties anaFilePatterns) =
        object [ "name"          .= anaName
               , "properties"    .= anaProperties
               , "file_patterns" .= anaFilePatterns ]

-- Default Analysis Name, Properties, File Patterns.
anaDefaultName         = "Issue"
anaDefaultProperties   = [ apTypes, apPriorities, apLabels ]
anaDefaultFilePatterns = []

-- Analysis properties available by default.
apTypes = AnalysisProperty
    { apName   = "type"
    , apType   = "string"
    , apValue  = [ "bug", "feature", "improvement", "task" ]
    , apPolicy = "enforcing" }

apPriorities = AnalysisProperty
    { apName   = "priority"
    , apType   = "string"
    , apValue  = [ "low", "normal", "high" ]
    , apPolicy = "enforcing" }

apLabels = AnalysisProperty
    { apName   = "labels"
    , apType   = "string"
    , apValue  = [ "data", "performance", "security", "testing", "user experience" ]
    , apPolicy = "permissive" }

-- From/To JSON definitions for AnalysisProperty.
instance FromJSON AnalysisProperty where
    parseJSON (Object v) = AnalysisProperty
                           <$> v .: "name"
                           <*> v .: "type"
                           <*> v .: "value"
                           <*> v .: "policy"
    parseJSON invalid    = typeMismatch "AnalysisProperty" invalid

instance ToJSON AnalysisProperty where
    toJSON (AnalysisProperty apName apType apValue apPolicy) =
        object [ "name"   .= apName
               , "type"   .= apType
               , "value"  .= apValue
               , "policy" .= apPolicy ]

-- Defualt Build configuration.
bDefault = Build
    { bRules = rDefault }

-- From/To JSON definitions for Build.
instance FromJSON Build where
    parseJSON (Object v) = Build
                           <$> v .:? "rules" .!= rDefault
    parseJSON invalid    = typeMismatch "Build" invalid

instance ToJSON Build where
    toJSON (Build bRules) =
        object [ "rules" .= bRules ]

-- Default Rules.
rDefault = [ rPassing, rWarning, rFailing ]

-- Rules available by default.
rPassing = Rule
    { rOperator   = "nop"
    , rConditions = [ condPassing ]
    , rActions    = [ acStatusPassing ] }

rWarning = Rule
    { rOperator   = "and"
    , rConditions = [ condWarningMinimum, condWarningMaximum ]
    , rActions    = [ acStatusWarning ] }

rFailing = Rule
    { rOperator   = "nop"
    , rConditions = [ condFailing ]
    , rActions    = [ acStatusFailing ] }

-- From/To JSON definitions for Rule.
instance FromJSON Rule where
    parseJSON (Object v) = Rule
                           <$> v .:? "operator"   .!= "nop"
                           <*> v .:  "conditions"
                           <*> v .:  "actions"
    parseJSON invalid    = typeMismatch "Rule" invalid

instance ToJSON Rule where
    toJSON (Rule rOperator rConditions rActions) =
        object [ "operator"   .= rOperator
               , "conditions" .= rConditions
               , "actions"    .= rActions ]

-- Conditions available by default.
condPassing = Condition
    { condType       = "maximum"
    , condCount      = 0
    , condOperator   = "and"
    , condProperties = [cpTypeBug, cpPriorityHigh] }

condWarningMinimum = Condition
    { condType       = "minimum"
    , condCount      = 1
    , condOperator   = "and"
    , condProperties = [cpTypeBug, cpPriorityHigh] }

condWarningMaximum = Condition
    { condType       = "maximum"
    , condCount      = 3
    , condOperator   = "and"
    , condProperties = [cpTypeBug, cpPriorityHigh] }

condFailing = Condition
    { condType       = "minimum"
    , condCount      = 4
    , condOperator   = "and"
    , condProperties = [cpTypeBug, cpPriorityHigh] }

-- From/To JSON definitions for Condition.
instance FromJSON Condition where
    parseJSON (Object v) = Condition
                           <$> v .: "type"
                           <*> v .: "count"
                           <*> v .: "operator"
                           <*> v .: "properties"
    parseJSON invalid    = typeMismatch "Condition" invalid

instance ToJSON Condition where
    toJSON (Condition condType condCount condOperator condProperties) =
        object [ "type"       .= condType
               , "count"      .= condCount
               , "operator"   .= condOperator
               , "properties" .= condProperties ]

-- Condition properties available by default.
cpTypeBug = ConditionProperty
    { cpName  = "type"
    , cpValue = "bug" }

cpPriorityHigh = ConditionProperty
    { cpName  = "priority"
    , cpValue = "high" }

-- From/To JSON definitions for ConditionProperty.
instance FromJSON ConditionProperty where
    parseJSON (Object v) = ConditionProperty
                           <$> v .: "name"
                           <*> v .: "value"
    parseJSON invalid    = typeMismatch "ConditionProperty" invalid

instance ToJSON ConditionProperty where
    toJSON (ConditionProperty cpName cpValue) =
        object [ "name"  .= cpName
               , "value" .= cpValue ]


-- Actions of type "status" available by default.
acStatusPassing = Action
   { acType = "status"
   , acData = ActionDataStatus { adsName = "passing" } }

acStatusWarning = Action
   { acType = "status"
   , acData = ActionDataStatus { adsName = "warning" } }

acStatusFailing = Action
   { acType = "status"
   , acData = ActionDataStatus { adsName = "failing" } }

-- From/To JSON definitions for Action.
instance FromJSON Action where
    parseJSON (Object v) = Action
                           <$> v .: "type"
                           <*> v .: "data"
    parseJSON invalid    = typeMismatch "Action" invalid

instance ToJSON Action where
    toJSON (Action acType acData) =
        object [ "type" .= acType
               , "data" .= acData ]

-- From/To JSON definitions for ActionData.
instance FromJSON ActionData where
    parseJSON (Object v) = ActionDataStatus
                           <$> v .: "name"
    parseJSON invalid    = typeMismatch "ActionDataStatus" invalid

instance ToJSON ActionData where
    toJSON (ActionDataStatus adsName) =
        object [ "name" .= adsName ]
