{-# LANGUAGE OverloadedStrings #-}

module Unhack.Parser.Version2
    ( parse
    ) where


-- Imports.

-- External dependencies.

import Control.Applicative
import Control.Monad       (unless, when)
import Control.Monad.State
import Data.Maybe          (fromJust, isJust, isNothing)

import qualified Data.Attoparsec.Text as AP
import qualified Data.Char            as C
import qualified Data.List            as L (elem, lookup, null)
import qualified Data.Text            as T (concat, dropAround, dropEnd, isSuffixOf, null, stripEnd, Text)

-- Internal dependencies.

import Unhack.Util (csvToList)

import qualified Unhack.Data.IssueProperties as UDIP (IssueProperties(..))


-- Public API.

-- Given a text, parse it for annotations and return them.
{-
  @I Receive the input text as is and split it into lines internally

     type     : improvement
     priority : normal
     labels   : 1.0.0-beta1, parser

  @I Return the list of annotations (IssueProperties records) as the result of the computation

     type     : improvement
     priority : normal
     labels   : 1.0.0-beta1, parser
-}
parse :: [T.Text] -> ParsingState
parse textLines = issueProperties
    where (issueProperties, state) = runState (parseLines textLines) initialParsingState


-- Functions/types for internal use.

-- To keep function names short we use the following as shortcut prefixes,
-- infixes or suffixes, throughout this module:
-- 'a'   for 'annotation'
-- 'c'   for 'current'
-- 'ca'  for 'currentAnnotation'
-- 'cp'  for 'currentProperty'
-- 'np'  for 'newProperty'
-- 'p'   for 'property'
-- 's'   for 'state'
-- 'sa'  for 'stateAnnotation'

-- Types for holding the state while parsing the input text.

data ParsingState = ParsingState
    { annotations       :: [UDIP.IssueProperties]
    , currentAnnotation :: Maybe Annotation
    , currentProperty   :: Maybe Property
    } deriving (Show)

data Annotation = Annotation
    { foundProperties     :: Maybe [Property]
    , availableProperties :: [Property]
    } deriving (Show)

type Property = (PropertyName, PropertyState)

newtype PropertyName = PropertyName T.Text deriving (Eq, Show)

data PropertyState = PropertyState
    { status :: PropertyStatus
    , type'  :: PropertyType
    , value  :: Maybe T.Text
    } deriving (Show)

data PropertyStatus
    = Available
    | Initiated
    | Terminated
    deriving (Eq, Show)

{-
  @I The heading could be any property type

     type     : improvement
     priority : low
     labels   : parser

     notes    : Either add 'isHeading' as a separate property field, or change
                PropertyType data constructors to receive a Bool argument.
-}
data PropertyType
    = Heading
    | SingleValue
    | MultiValue
    | SingleParagraph
    | MultiParagraph
    deriving (Eq, Show)

-- For every given input, we will be starting with an empty state.
initialParsingState = ParsingState
    { annotations       = []
    , currentAnnotation = Nothing
    , currentProperty   = Nothing
    }

-- The initial state annotation (found + available properties) that is used when
-- we detect an annotation in the input text.
{-
  @I Read the available property types from the config

     type     : improvement
     priority : normal
     labels   : 1.0.0, parser
-}
initialAnnotation = Annotation
    { foundProperties     = Nothing
    , availableProperties =
        [ ( PropertyName "title"
          , PropertyState { status = Available
                          , type'  = Heading
                          , value  = Nothing
                          }
          )
        , ( PropertyName "type"
          , PropertyState { status = Available
                          , type'  = SingleValue
                          , value  = Nothing
                          }
          )
        , ( PropertyName "priority"
          , PropertyState { status = Available
                          , type'  = SingleValue
                          , value  = Nothing
                          }
          )
        , ( PropertyName "labels"
          , PropertyState { status = Available
                          , type'  = MultiValue
                          , value  = Nothing
                          }
          )
        , ( PropertyName "notes"
          , PropertyState { status = Available
                          , type'  = MultiParagraph
                          , value  = Nothing
                          }
          )
        ]
    }

-- Parse the given list of text lines for annotations. This is a recursive
-- function that delegates the actual parsing of each line to other functions
-- depending on whether we have already detected an annotation and we are in the
-- middle of processing it, or not.
parseLines :: [T.Text] -> State ParsingState ParsingState

parseLines [] = do
    state <- get
    return state

parseLines (current:remaining) = do
    state@(ParsingState annotations' maybeCA maybeCP) <- get
    maybe (doesNotHaveCA $ trimLine current) (hasCA (trimLine current) maybeCP) maybeCA
    parseLines remaining

-- Function that parses a line when we are not already inside an annotation.
-- @I Terminate the annotation when there is only one known property, and that
--    property is terminated in the heading
--
--    type     : bug
--    priority : low
--    labels   : parser
doesNotHaveCA :: T.Text -> State ParsingState ParsingState
doesNotHaveCA inputLine

    {-
      @I Terminate the annotation if we have unique unterminated property that
         is not of SingleParagraph type

         type     : bug
         priority : low
         labels   : parser
    -}

    | not isAnnotation                      = get
    | isAnnotation && isNothing maybeP      = state $ \s -> (sInitiateA  s, sInitiateA  s)
    | isAnnotation && aHasTerminator        = state $ \s -> (sTerminateA s, sTerminateA s)
    | isAnnotation && pStatus == Initiated  = state $ \s -> (sInitiateP  s, sInitiateP  s)
    | isAnnotation && pStatus == Terminated = state $ \s -> (sTerminateP s, sTerminateP s)

    where
        (isAnnotation, maybeP) = parseAInitiation inputLine

        (lineIsProperty, lineProperty) = maybe (False, Nothing) parseP maybeP

        -- The 'pSomething' functions hold the values of the property that
        -- accompanies the annotation initiator. They should be called to
        -- evaluation only with the knowledge that there is a property on the
        -- same line as the annotation initiator.
        pName
            | lineIsProperty = fst $ fromJust lineProperty
            | otherwise      = PropertyName "title"
        pValue
            | lineIsProperty = snd $ fromJust lineProperty
            | otherwise      = fromJust maybeP

        pType = pTypeByName pName

        pStatus
           | pIsTerminated  = Terminated
           | isValueP pType = Terminated
           | otherwise      = Initiated

        pIsTerminated  = pHasTerminator || aHasTerminator
        pHasTerminator = hasPTerminator pValue
        aHasTerminator = hasATerminator pValue

        -- Remove terminators' strings from the property value.
        pValueTrimmed = trimPValue pHasTerminator aHasTerminator pValue

        currentP
            = ( pName
              , PropertyState { status = pStatus
                              , type'  = pType
                              , value  = Just pValueTrimmed
                              }
              )

        -- The annotations as IssueProperties records that will be added to the
        -- state. We currently ignore the annotation if there is no 'title' available.
        newSAnnotations
            | titleExists = [saToProperties $ (addFoundP currentP) initialAnnotation]
            | otherwise   = []

            where titleExists = (pTypeByName $ fst currentP) == Heading

        -- States.

        -- Computation for initiating the annotation.
        -- When there is no heading or property accompanying the annotation
        -- initiator, we do not set a current property - it will be detected in
        -- the next line.
        sInitiateA s = s { currentAnnotation = Just initialAnnotation }

        -- Computation for initiating the annotation and the property.
        sInitiateP s = s
            { currentAnnotation = Just $ (removeAvailableP pName) initialAnnotation
            , currentProperty   = Just currentP
            }

        -- Computation for initiating the annotation and terminating the property.
        sTerminateP s = s
            { currentAnnotation = Just $ (removeAvailableP pName) . (addFoundP currentP) $ initialAnnotation
            }

        -- Computation for terminating the annotation, and the property if there was one.
        sTerminateA s = s
            { annotations       = newSAnnotations ++ annotations s
            , currentAnnotation = Nothing
            , currentProperty   = Nothing
            }

-- Function that parses a line when we are not already inside an annotation.
hasCA :: T.Text -> Maybe Property -> Annotation -> State ParsingState ParsingState
hasCA inputLine maybeCP currentA

    | aIsTerminated                       = state $ \s -> (sTerminateA               s, sTerminateA               s)
    | cpIsContinued  && cpIsTerminated    = state $ \s -> (sContinueAndTerminateCP   s, sContinueAndTerminateCP   s)
    | cpIsContinued                       = state $ \s -> (sContinueCP               s, sContinueCP               s)
    | cpIsTerminated && npIsTerminated    = state $ \s -> (sTerminateCPAndNP         s, sTerminateCPAndNP         s)
    | cpIsTerminated && npIsInitiated     = state $ \s -> (sTerminateCPAndInitiateNP s, sTerminateCPAndInitiateNP s)
    | cpIsTerminated                      = state $ \s -> (sTerminateCP              s, sTerminateCP              s)
    | npIsTerminated                      = state $ \s -> (sTerminateNP              s, sTerminateNP              s)
    | npIsInitiated                       = state $ \s -> (sInitiateNP               s, sInitiateNP               s)
    | otherwise                           = get

    where
        isEmptyLine = T.null inputLine

        (cpName, cpState) = fromJust maybeCP

        cpIsSingleParagraph
            | type' cpState == Heading         = True
            | type' cpState == SingleParagraph = True
            | otherwise                        = False

        (lineIsProperty, lineProperty) = parseP inputLine

        npName
            | lineIsProperty = fst $ fromJust lineProperty
            | otherwise      = cpName
        npValue
            | lineIsProperty = snd $ fromJust lineProperty
            -- A property's state value can be Nothing, but a property would always have a value if
            -- it is set to current. We can therefore safely use 'fromJust' to extract the value
            -- from the current property's state
            | otherwise      = T.concat [fromJust $ value cpState, " ", inputLine]

        npType = pTypeByName npName

        -- We set the new property's state to Initiated here. If the property is
        -- terminated, it will be set to Terminated in the returned state.
        npStatus = Initiated

        npHasTerminator = hasPTerminator npValue
        aHasTerminator  = hasATerminator npValue

        -- We remove the property/annotation terminators from the new property,
        -- if present.
        npValueTrimmed = trimPValue npHasTerminator aHasTerminator npValue

        npState = PropertyState
            { status = npStatus
            , type'  = npType
            , value  = Just npValueTrimmed
            }

        newP = (npName, npState)

        -- The current property with its 'status' set to 'Terminated'.
        terminatedCP = (cpName, cpState { status = Terminated })

        -- The new property with its 'status' set to 'Terminated'.
        terminatedNP = (npName, npState { status = Terminated })

        -- The annotations as IssueProperties records that will be added to the
        -- state. We currently ignore the annotation if there is no 'title' available.
        newSAnnotations
            | titleExists = [saToProperties updatedA]
            | otherwise   = []

            where titleExists = isJust $ maybe Nothing (lookup (PropertyName "title")) $ foundProperties updatedA

        {-
          @I Take into account multi-paragraph current properties

             type     : bug
             priority : normal
             labels   : parser
        -}

        -- Indicates that the current property (from previous line) is continued
        -- in this line.
        cpIsContinued
            | isNothing maybeCP = False
            | isEmptyLine       = False
            | lineIsProperty    = False
            | otherwise         = True

        -- Indicates that the current property is terminated, whether it was
        -- continued in this line or not.
        cpIsTerminated
            | isNothing maybeCP                  = False
            | npIsInitiated                      = True
            | isEmptyLine && cpIsSingleParagraph = True
            | npHasTerminator || aHasTerminator  = True
            | otherwise                          = False

        -- Indicates that a new property is initiated. It does not include the
        -- case when this line is a continuation of the current property.
        npIsInitiated
            | isEmptyLine                                                 = False
            | lineIsProperty                                              = True
            | isNothing maybeCP && (isNothing $ foundProperties currentA) = True
            | otherwise                                                   = False

        -- Indicates that the new property is terminated. It does not include
        -- the case where this line is termination of the current property.
        npIsTerminated
            | not npIsInitiated = False
            | isValueP npType   = True
            | npHasTerminator   = True
            | aHasTerminator    = True
            | otherwise         = False

        -- Indicates that thew current annotation is terminated. It includes all
        -- cases where the annotation is temrinated, such as when encountering
        -- an annotation terminator, when all available properties have been
        -- found, and when we encounter free text that does not belong to a
        -- property.
        aIsTerminated
            | cpIsContinued  && aHasTerminator    = True
            | npIsInitiated  && aHasTerminator    = True
            | cpIsTerminated && not npIsInitiated = foundAllP [cpName]         currentA
            | cpIsTerminated && npIsTerminated    = foundAllP [cpName, npName] currentA
            | npIsTerminated                      = foundAllP [npName]         currentA

            -- If we have a free-text line that is not a continuation of the
            -- current property, or an initiation of the heading, then it must
            -- be that the annotation is over.
            | not (isEmptyLine || lineIsProperty || cpIsContinued || npIsInitiated) = True

            | otherwise                           = False

        -- The updated version of the current Annotation record, after parsing
        -- the current line, which will be used to get the final annotation
        -- properties (IssueProperties record) that will be added to the state
        -- when terminating the current annotation. Should only be used with the
        -- knowledge that we are terminating annotations.
        updatedA
            | cpIsTerminated && cpIsContinued    = addFoundP terminatedNP currentA
            | cpIsTerminated && npIsTerminated   = (addFoundP terminatedCP)  .
                                                   (removeAvailableP npName) .
                                                   (addFoundP terminatedNP)  $ currentA
            | cpIsTerminated                     = addFoundP terminatedCP currentA
            | npIsTerminated                     = (removeAvailableP npName) .
                                                   (addFoundP terminatedNP)  $ currentA
            | otherwise                          = currentA

        -- States

        -- Computation for terminating the current property.
        sTerminateCP s = s
            { currentAnnotation = Just $ addFoundP terminatedCP currentA
            , currentProperty   = Nothing
            }

        -- Computation for continuing the current property.
        sContinueCP s = s
            { currentAnnotation = Just currentA
            , currentProperty   = Just newP
            }

        -- Computation for continuing and terminating the current property.
        sContinueAndTerminateCP s = s
            { currentAnnotation = Just $ addFoundP terminatedNP currentA
            , currentProperty   = Nothing
            }

        -- Computation for terminating the current property and initiating the new one.
        sTerminateCPAndInitiateNP s = s
            { currentAnnotation = Just $ (addFoundP terminatedCP)  .
                                         (removeAvailableP npName) $ currentA
            , currentProperty   = Just newP
            }

        -- Computation for terminating both the current and the new property.
        sTerminateCPAndNP s = s
            { currentAnnotation = Just $ (addFoundP terminatedCP)  .
                                         (removeAvailableP npName) .
                                         (addFoundP terminatedNP)  $ currentA
            , currentProperty   = Nothing
            }

        -- Computation for initiating the new property.
        sInitiateNP s = s
            { currentAnnotation = Just $ removeAvailableP npName currentA
            , currentProperty   = Just newP
            }

        -- Computation for terminating the new property.
        sTerminateNP s = s
            { currentAnnotation = Just $ (removeAvailableP npName) .
                                         (addFoundP terminatedNP)  $ currentA
            , currentProperty   = Nothing
            } 

        -- Computation for terminating the annotation.
        sTerminateA s = s
            { annotations       = annotations s ++ newSAnnotations
            , currentAnnotation = Nothing
            , currentProperty   = Nothing
            }

-- Helper functions used while parsing lines.

-- Convert the found properties in the given state annotation to an IssueProperties record.
saToProperties :: Annotation -> UDIP.IssueProperties
saToProperties annotation
    = UDIP.IssueProperties { UDIP.labels   = maybe Nothing (Just . csvToList) $ maybe Nothing extractLabels foundProperties'
                           , UDIP.notes    = maybe Nothing extractNotes foundProperties'
                           , UDIP.priority = maybe Nothing extractPriority foundProperties'
                           -- @I Consider what should happen if a title is not present in an annotation ~:]
                           , UDIP.title    = fromJust $ maybe Nothing extractTitle foundProperties'
                           , UDIP.type'    = maybe Nothing extractType foundProperties'
                           }
    where foundProperties' = foundProperties annotation
          extractLabels    = pValueFromList (PropertyName "labels")
          extractNotes     = pValueFromList (PropertyName "notes")
          extractPriority  = pValueFromList (PropertyName "priority")
          extractTitle     = pValueFromList (PropertyName "title")
          extractType      = pValueFromList (PropertyName "type")

-- Given a list of property names and an annotation (which contains the properties not found yet),
-- calculate whether all given properties have been found yet or not.
foundAllP :: [PropertyName] -> Annotation -> Bool
foundAllP propertiesNames annotation = L.null $ filter (\ (availablePName, availablePState)
                                                          -> not (elem availablePName propertiesNames))
                                                       $ availableProperties annotation

-- Given a list of properties, get the value of the property with the given name.
pValueFromList :: PropertyName -> [Property] -> Maybe T.Text
pValueFromList propertyName foundProperties' = maybe Nothing value maybePropertyState
    where maybePropertyState = L.lookup propertyName foundProperties'

-- Remove the property with the given name from the available properties of given state annotation.
removeAvailableP :: PropertyName -> Annotation -> Annotation
removeAvailableP propertyName annotation
    = annotation { availableProperties = filter (\ (availablePName, availablePState) -> propertyName /= availablePName)
                                                $ availableProperties annotation
                 }

-- Add the given property to the found properties of the given state annotation.
addFoundP :: Property -> Annotation -> Annotation
addFoundP property annotation
    = annotation { foundProperties = Just $ maybe [property] ((++) [property]) $ foundProperties annotation
                 }

-- Parsing functions for detecting an annotation initiation or a property in a
-- given input text.

parseAInitiation :: T.Text -> (Bool, Maybe T.Text)
parseAInitiation text = interpretAResult $ AP.parse aInitiationParser text

interpretAResult :: AP.Result T.Text -> (Bool, Maybe T.Text)
interpretAResult (AP.Fail _ _ _)   = (False, Nothing)
interpretAResult (AP.Partial _)    = (False, Nothing)
interpretAResult (AP.Done property _)
    | T.null property = (True, Nothing)
    | otherwise       = (True, Just property)

parseP :: T.Text -> (Bool, Maybe (PropertyName, T.Text))
parseP text = interpretPResult $ AP.parse pParser text

interpretPResult :: AP.Result T.Text -> (Bool, Maybe (PropertyName, T.Text))
interpretPResult (AP.Fail _ _ _)   = (False, Nothing)
interpretPResult (AP.Partial _)    = (False, Nothing)
interpretPResult (AP.Done value' name')
    | isValidPName name' = (True, Just (PropertyName name', value'))
    | otherwise          = (False, Nothing)

-- Aeson Parsers.

{-
  @I When the first property is at the next line after the annotation initiator,
     the annotation is not detected because of the missing space after '@I'

     type     : bug
     priority : normal
     labels   : 1.0.0-beta1, parser
-}
aInitiationParser :: AP.Parser T.Text
aInitiationParser = (AP.string aInitiator <|> AP.string aInitiatorShortcut) <* (AP.takeWhile C.isSpace)

pParser :: AP.Parser T.Text
pParser = AP.takeWhile (not . C.isSpace) <* (AP.takeWhile C.isSpace) <* AP.char ':' <* (AP.takeWhile C.isSpace)

-- Indicators of whether an annotation/property is initiating or terminating.
{-
  @I Read initiator and terminator strings from the config

     type     : improvement
     priority : normal
     labels   : 1.0.0, parser
-}
aInitiator         = "@Issue "
aInitiatorShortcut = "@I "
pTerminator        = "~]"
aTerminator        = "~:]"
pTerminatorLength  = 2
aTerminatorLength  = 3

hasATerminator :: T.Text -> Bool
hasATerminator = T.isSuffixOf aTerminator

hasPTerminator :: T.Text -> Bool
hasPTerminator = T.isSuffixOf pTerminator

-- Helper functions for working with properties.

isValueP :: PropertyType -> Bool
isValueP SingleValue = True
isValueP MultiValue  = True
isValueP _           = False

isValidPName :: T.Text -> Bool
isValidPName name'
    | name' == "labels"   = True
    {-
      @I Reinstate 'notes' as a valid property when MultiParagraph properties
         are supported

         type     : bug
         priority : normal
         labels   : parser
    -}
--    | name' == "notes"    = True
    | name' == "priority" = True
    | name' == "title"    = True
    | name' == "type"     = True
    | otherwise           = True

pTypeByName :: PropertyName -> PropertyType
pTypeByName (PropertyName propertyName)
    | propertyName == "title"    = Heading
    | propertyName == "type"     = SingleValue
    | propertyName == "priority" = SingleValue
    | propertyName == "labels"   = MultiValue
    | propertyName == "notes"    = MultiParagraph

-- Helper functions for trimming text lines from whitespace and comment characters before parsing
-- them.

-- Removes the property or annotation terminator string from the end of the
-- given property's value.
trimPValue :: Bool -> Bool -> T.Text -> T.Text
trimPValue pHasTerminator aHasTerminator pValue
    | pHasTerminator = T.stripEnd $ T.dropEnd pTerminatorLength pValue
    | aHasTerminator = T.stripEnd $ T.dropEnd aTerminatorLength pValue
    | otherwise      = pValue

-- Removes any spaces and comment characters from the beginning and end of an input line.
trimLine :: T.Text -> T.Text
trimLine text = T.dropAround characterShouldBeTrimmed text
    where characterShouldBeTrimmed character = C.isSpace character || L.elem character commentCharacters

-- A list of comment characters that should be removed from the beginning and
-- end of input lines.
commentCharacters :: [Char]
commentCharacters = ['*', '/', '-', '{', '}']
