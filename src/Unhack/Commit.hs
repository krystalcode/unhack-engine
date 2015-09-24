{-# LANGUAGE OverloadedStrings #-}

module Unhack.Commit
       ( stringToCommit
       , Commit(..)
       ) where

import Data.List.Split

stringToCommit :: String -> Commit
stringToCommit input = Commit { hash = (commitAsList input) !! 0
                              -- Convert to exact ISO 8601 as expected by ElasticSearch.
                              , time = day ++ "T" ++ timeAndTimezone }
    where commitAsList string = splitOn "_" string
          date = filter (/=' ') $ (commitAsList input) !! 1
          (day, timeAndTimezone) = splitAt 10 date
          

data Commit = Commit { hash :: String
                     , time :: String
                     } deriving (Show)
