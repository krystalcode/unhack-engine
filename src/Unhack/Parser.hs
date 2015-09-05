module Unhack.Parser
       ( parseFile
       ) where

import Unhack.Issue
import Data.List
import Data.List.Split
import Text.Regex.PCRE

-- Public API.
parseFile :: [String] -> [Issue]
parseFile issues = map extractProperties issues

-- Functions for internal use.
extractIssues :: String -> [String]
extractIssues input = trimIssues . regexAllMatches $ input

regexAllMatches :: String -> [String]
regexAllMatches input = getAllTextMatches $ input =~ "@Issue\\(([\\s\\S]+?)\\)" :: [String]

trimIssues :: [String] -> [String]
trimIssues xs = map (takeWhile (/=')') . tail . dropWhile (/='(')) xs

extractProperties :: String -> Issue
extractProperties issue = Issue { projectId = "AU8urrJaVfWpfA7E_XUN"
                                , commit = "fa253171b27f2c31b933f63a9be439922b6f9da8"
                                , title = propertyList !! 0
                                , kind = propertyList !! 1
                                , priority = propertyList !! 2
                                , labels = propertyList !! 3 }
                  where issueProperties = ["title", "type", "priority", "labels"]
                        propertyList = map (extractProperty issue) issueProperties

extractProperty :: String -> String -> String
extractProperty issue property = stripNewLines . trimProperty $ (issue =~ pattern :: String)
                where pattern = property ++ "=\"([^\"]+)\""

stripNewLines :: String -> String
stripNewLines [] = []
stripNewLines property = intercalate " " . map (trimLineBeginning) $ splitLines
              where splitLines = splitOn "\n" property

trimLineBeginning :: String -> String
trimLineBeginning [] = []
trimLineBeginning line = dropWhile(`elem` " /*") line

trimProperty :: String -> String
trimProperty [] = []
trimProperty property = takeWhile (/='"') . tail . dropWhile (/='"') $ property
