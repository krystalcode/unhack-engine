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
directory repository = T.unpack $ T.intercalate "/" [base, vendor, owner, name]
    where vendor   = UDR.vendor repository
          fullName = UDR.name repository

          {-
            @Issue(
              "Store the repository name in the database when creating it"
              type="improvement"
              priority="low"
              labels="release"
            )
          -}
          nameParts = T.split (=='/') fullName
          owner     = nameParts !! 1
          name      = nameParts !! 2

base :: T.Text
base = "/repositories"
