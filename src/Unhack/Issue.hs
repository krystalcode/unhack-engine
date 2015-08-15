module Unhack.Issue
       ( Issue(..)
       ) where

data Issue = Issue { title :: String
                   , kind :: String
                   , priority :: String
                   , labels :: String
                   } deriving (Show)
