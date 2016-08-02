{-# LANGUAGE OverloadedStrings #-}

module Unhack.Build.Condition
       ( areMet
       , isMet
       ) where


-- Imports.

import qualified Data.Text     as T   (unpack, Text)
import qualified Unhack.Config as UC  (Condition(..), ConditionProperty(..))
import qualified Unhack.Issue  as UDI (accessListProperty, accessProperty, propertyStringToList, Issue)


-- Public API.

-- Calculates whether a list of Issues meet a list of Conditions, joined by the given operator.
areMet :: [UC.Condition] -> T.Text -> [UDI.Issue] -> Bool
areMet conditions operator issues
    | operator == "and"      = not . elem False $ isMetList
    | operator == "or"       = elem True isMetList
    -- Default to the "and" operator otherwise. This can happen if we have only one condition, or if there is a typo in
    -- the configuration.
    {-
      @Issue(
        "Providing a default operator in case of error should happen when loading the configuration"
        type="bug"
        priority="normal"
      )
    -}
    | otherwise = not . elem False $ isMetList
    where isMetList = map (isMet' issues) conditions
          isMet'    = flip isMet

-- Calculates whether a list of Issues meet a Condition.
isMet :: UC.Condition -> [UDI.Issue] -> Bool
isMet condition issues
    {-
      @Issue(
        "Implement validation that ensures the condition type is one of the supported ones"
        type="bug"
        priority="normal"
      )
    -}
    | _type == "minimum" = realCount >= requiredCount
    | _type == "maximum" = realCount <= requiredCount
    where realCount     = propertiesCount properties operator issues
          requiredCount = UC.condCount condition
          operator      = UC.condOperator condition
          properties    = UC.condProperties condition
          _type         = UC.condType condition


-- Functions/types for internal use.

-- Calculate the count of Issues in the given list that match the criteria in the given condition properties and
-- operator.
propertiesCount :: [UC.ConditionProperty] -> T.Text -> [UDI.Issue] -> Int
propertiesCount properties operator issues = length $ filter (filterBy operator) issues
    where -- Convert the condition properties to a list of filtering functions.
          filters = map makeFilter properties
          makeFilter property
              | name == "labels" = propertyContains name value
              | otherwise        = propertyEquals name value
              where name  = UC.cpName property
                    value = UC.cpValue property

          -- Run all filters on the given Issue.
          runFilters issue = map (\x -> x issue) filters

          -- Calculate the final result for an Issue, depending on the given operator.
          filterBy operator issue
              | operator == "and"   = not . elem False $ runFilters issue
              | operator == "or"    = elem True $ runFilters issue
              -- Default to the "and" operator otherwise. This can happen if we have only one property, or if there is a
              -- typo in the configuration.
              {-
                @Issue(
                  "Providing a default operator in case of error should happen when loading the configuration"
                  type="bug"
                  priority="normal"
                )
              -}
              | otherwise = not . elem False $ runFilters issue

          -- Returns whether the annotation has the property with the given name and value.
          propertyEquals name value issue = T.unpack value == UDI.accessProperty issue (T.unpack name)

          -- Returns whether the annotation has the property with the given name and a value that contains the given
          -- value. This is relevant to multi-value properties.
          propertyContains name value issue = T.unpack value `elem` (UDI.accessListProperty issue (T.unpack name))
