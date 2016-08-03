{-# LANGUAGE OverloadedStrings #-}

module Unhack.Util
    ( csvToList
    , omitNulls
    , posixToUTC
    ) where


-- Imports.

import Data.Aeson            (object, Value(Null))
import Data.Char             (isSpace)
import Data.Time             (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import qualified Data.Text as T (dropWhile, dropWhileEnd, null, splitOn, unpack, Text)


-- Public API.

-- Convert a multi-value text property from a comma-separated value to a list.
csvToList :: T.Text -> [T.Text]
csvToList input = filter (not . T.null) trimmed
    where list    = T.splitOn "," input
          trimmed = map (T.dropWhile isSpace . T.dropWhileEnd isSpace) list

-- Filter out null values from a JSON object. It can be used to ensure that
-- record fields with value 'Nothing' will not be sent to Elastic Search for
-- indexing.
omitNulls :: [(T.Text, Value)] -> Value
omitNulls = object . filter notNull
    where notNull (_, Null) = False
          notNull _         = True

-- Convert a posix (unix) timestamp, given as text, to UTCTime.
posixToUTC :: T.Text -> UTCTime
posixToUTC timestamp = posixSecondsToUTCTime $ realToFrac integer
    where integer = read (T.unpack timestamp) :: Integer
