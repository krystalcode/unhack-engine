module Unhack.Issue
       ( Issue(..)
       ) where

data Issue = Issue { projectId :: String
                   , commit :: String
                   , title :: String
                   , kind :: String
                   , priority :: String
                   , labels :: String
                   } deriving (Show)
