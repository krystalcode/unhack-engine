{-# LANGUAGE OverloadedStrings #-}

module Unhack.Build.Condition
       ( areMet
       , isMet
       ) where


-- Imports.

-- External dependencies.

import Data.Maybe (fromJust, isNothing)

import qualified Data.Text as T (Text)

-- Internal dependencies.

import qualified Unhack.Config               as UC   (Condition(..), ConditionProperty(..))
import qualified Unhack.Data.IssueProperties as UDIP (accessMultiValueProperty, accessSingleValueProperty, IssueProperties)


-- Public API.

-- Calculates whether a list of Issues (given as IssueProperties records) meet a
-- list of Conditions, joined by the given operator.
areMet :: [UC.Condition] -> T.Text -> [UDIP.IssueProperties] -> Bool
areMet conditions operator issuesProperties
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
    where isMetList = map (isMet' issuesProperties) conditions
          isMet'    = flip isMet

-- Calculates whether a list of Issues (given as IssueProperties records) meet a
-- Condition.
isMet :: UC.Condition -> [UDIP.IssueProperties] -> Bool
isMet condition issuesProperties
    {-
      @Issue(
        "Implement validation that ensures the condition type is one of the supported ones"
        type="bug"
        priority="normal"
      )
    -}
    | _type == "minimum" = realCount >= requiredCount
    | _type == "maximum" = realCount <= requiredCount
    where realCount     = propertiesCount properties operator issuesProperties
          requiredCount = UC.condCount condition
          operator      = UC.condOperator condition
          properties    = UC.condProperties condition
          _type         = UC.condType condition


-- Functions/types for internal use.

-- Calculate the count of Issues in the given list that match the criteria in the given condition properties and
-- operator.
propertiesCount :: [UC.ConditionProperty] -> T.Text -> [UDIP.IssueProperties] -> Int
propertiesCount properties operator issuesProperties = length $ filter (filterBy operator) issuesProperties
    where -- Convert the condition properties to a list of filtering functions.
          filters = map makeFilter properties
          makeFilter property
              | name == "labels" = propertyContains name value
              | otherwise        = propertyEquals name value
              where name  = UC.cpName property
                    value = UC.cpValue property

          -- Run all filters on the given Issue.
          runFilters issueProperties = map (\x -> x issueProperties) filters

          -- Calculate the final result for an Issue, depending on the given operator.
          filterBy operator issueProperties
              | operator == "and" = not . elem False $ runFilters issueProperties
              | operator == "or"  = elem True $ runFilters issueProperties
              -- Default to the "and" operator otherwise. This can happen if we have only one property, or if there is a
              -- typo in the configuration.
              {-
                @Issue(
                  "Providing a default operator in case of error should happen when loading the configuration"
                  type="bug"
                  priority="normal"
                )
              -}
              | otherwise = not . elem False $ runFilters issueProperties

          -- Returns whether the annotation has the property with the given name and value.
          propertyEquals name value issueProperties
              | isNothing maybeValue = False
              | otherwise            = value == fromJust maybeValue

              where maybeValue = UDIP.accessSingleValueProperty issueProperties name

          -- Returns whether the annotation has the property with the given name and a value that contains the given
          -- value. This is relevant to multi-value properties.
          propertyContains name value issueProperties
              | isNothing maybeValues = False
              | otherwise             = value `elem` fromJust maybeValues

              where maybeValues = UDIP.accessMultiValueProperty issueProperties name
