{-# LANGUAGE OverloadedStrings #-}

module Unhack.Git.Location
       ( base
       , directory ) where


-- Imports.

-- External dependencies.

import qualified Data.Text as T (intercalate, split, unpack, Text)

-- Internal dependencies.

import qualified Unhack.Data.Repository as UDR


-- Public API.

directory :: UDR.Repository -> FilePath
directory repository = T.unpack $ T.intercalate "/" [base, UDR.name repository]

base :: T.Text
base = "/repositories"
