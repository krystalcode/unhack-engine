module Unhack.Pubsub.Repository
       ( clone ) where


-- Imports.

import Data.Aeson (eitherDecode)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T (Text, split, unpack)
import Database.Bloodhound
import Network.HTTP.Client (responseBody)
import qualified Unhack.Data.Repository as UDR
import qualified Unhack.Git.Fetch as UGF (clone)
import qualified Unhack.Storage.ElasticSearch.Config as USEC (StorageConfig, StorageIndexSettings)
import qualified Unhack.Storage.ElasticSearch.Operations as USEO (getDocument')


-- Public API.

-- Pubsub task for cloning a repository.
{-
    @Issue(
        "Refactor fetching and extracting a document into a separate function"
        type="improvement"
        priority="normal"
        labels="refactoring"
    )
-}
clone :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> IO ()
clone config indexSettings repositoryId = do
    -- Get the repository from Elastic Search.
    response <- USEO.getDocument' config indexSettings (DocId repositoryId)

    -- Get the result from the body of the response.
    let body         = responseBody response
    let eitherResult = eitherDecode body :: Either String (EsResult UDR.Repository)
    let result       = either error id eitherResult

    -- Get the repository from the _source attribute of the result.
    let maybeRepository = fmap _source . foundResult $ result

    case (isNothing maybeRepository) of

        True  -> error $ "No repository found with id '" ++ (T.unpack repositoryId) ++ "'."

        False -> do
            let repository         = fromJust maybeRepository
            let vendor             = UDR.vendor repository
            let repositoryFullName = UDR.name repository

            {-
              @Issue(
                "Store the repository name in the database when creating it"
                type="improvement"
                priority="low"
                labels="release"
              )
            -}
            let nameParts      = T.split (=='/') repositoryFullName
            let owner          = nameParts !! 1
            let repositoryName = nameParts !! 2

            -- Clone the repository.
            {-
              @Issue(
                "Check if the repository is already cloned first and do a fetch
                instead"
                type="bug"
                priority="low"
              )
              @Issue(
                "Catch and log errors so that the program does not fail"
                type="bug"
                priority="low"
                labels="error handling, log management"
              )
              @Issue(
                "Catch access denied errors and flag the repository so that a
                deploy key can be added or a message can be displayed to the
                user"
                type="bug"
                priority="low"
              )
            -}
            cloneResult <- UGF.clone vendor owner repositoryName
            print $ concat ["Cloned repository '", (T.unpack repositoryFullName), "': ", (T.unpack cloneResult)]
