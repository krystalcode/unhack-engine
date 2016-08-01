{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Unhack.Types
       ( EntityType
       ) where


-- Imports.

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GHC.Generics     (Generic)


-- Public API.

data EntityType = User deriving (Generic, Show)


-- Functions/types for internal use.

instance FromJSON EntityType where
    parseJSON = withText "User" parseValue
        where parseValue "user" = pure User
              parseValue v = fail $ "Invalid User: " ++ show v

instance ToJSON EntityType where
    toJSON User = "user"
