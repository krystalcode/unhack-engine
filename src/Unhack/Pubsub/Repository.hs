{-# LANGUAGE OverloadedStrings #-}

module Unhack.Pubsub.Repository
       ( analyseAll
       , clone ) where


-- Imports.

import Data.Aeson (eitherDecode)
import Data.Maybe (fromJust, isNothing)
import qualified Data.List as L (intersect)
import qualified Data.Text as T (unpack, Text)
import Database.Bloodhound
import qualified Unhack.Config as UC (defaultConfigFile, load)
import qualified Unhack.Data.EmBranch as UDEB
import qualified Unhack.Data.EmIssueCommit as UDEIC (toCommits)
import qualified Unhack.Data.EmbeddedRepository as UDER
import qualified Unhack.Data.Repository as UDR
import qualified Unhack.Git.Branch as UGB (branchesList)
import qualified Unhack.Git.Commit as UGCom (getCommits)
import qualified Unhack.Git.Contents as UGCon (fileContents')
import qualified Unhack.Git.Fetch as UGF (clone)
import qualified Unhack.Git.Location as UGL (directory)
import qualified Unhack.Git.Tree as UGT (commitTree', treeGlobFilter')
import qualified Unhack.Issue as UI (bulkSetRepository)
import qualified Unhack.Parser as UP (parseCommitFileString')
import qualified Unhack.Storage.ElasticSearch.Config as USEC (indexSettingsFromConfig, StorageConfig, StorageIndexSettings)
import qualified Unhack.Storage.ElasticSearch.Data.Repository as USEDR (get, markAccessible, markProcessed)
import qualified Unhack.Storage.ElasticSearch.Operations as USEO (bulkIndexDocuments', bulkIndexIssues)


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

    -- Get the repository record.
    maybeRepository <- USEDR.get config indexSettings repositoryId

    case (isNothing maybeRepository) of

        True  -> error $ "No repository found with id '" ++ (T.unpack repositoryId) ++ "'."

        False -> do
            let repository         = fromJust maybeRepository
            let vendor             = UDR.vendor repository
            let owner              = UDR.vendorUsername repository
            let repositoryName     = UDR.vendorName repository

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

            -- If cloning the repository was successful, flag the repository as
            -- accessible.
            accessibleResponse <- USEDR.markAccessible config indexSettings repositoryId repository

            print $ concat ["Cloned repository '", (T.unpack $ UDR.name repository), "'"]

-- Analyse all commits for the active branches.
analyseAll :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> IO ()
analyseAll config indexSettings repositoryId = do

    -- Get the repository record.
    maybeRepository <- USEDR.get config indexSettings repositoryId

    case (isNothing maybeRepository) of

        True  -> error $ "No repository found with id '" ++ (T.unpack repositoryId) ++ "'."

        False -> do
            let repository     = fromJust maybeRepository
            let directory      = UGL.directory repository
            let activeBranches = map (UDEB.name) $ UDR.activeBranches repository

            case (length activeBranches) of

              0 -> print "Trying to analyse a repository that does not have any active branches."

              _ -> do
                  -- We first make a check that the branches set to be analysed
                  -- actually exist. The branches that will actually be analysed
                  -- are therefore the intersection of all repository branches
                  -- with the branches set to be analysed.
                  branchesList <- UGB.branchesList directory
                  let branchesToAnalyse = L.intersect activeBranches branchesList

                  -- Analyse all commits for each branch.
                  {-
                      @Issue(
                          "Consider setting a pubsub task per commit instead of
                          analysing all commits at one go"
                          type="bug"
                          priority="low"
                          labels="cpu, memory, testing"
                      )
                  -}
                  responses <- mapM (analyseBranchAll config repositoryId repository) branchesToAnalyse

                  -- Mark the repository as processed if the 'isProcessed' flag
                  -- is currently set to 'false'. This would normally be the
                  -- case if this is the first time that we are processing the
                  -- repository.
                  case (UDR.isProcessed repository) of

                      False -> do
                          processedResponse <- USEDR.markProcessed config indexSettings repositoryId repository
                          print processedResponse

                      True  -> print $ concat ["Analysed all commits for repository '", (T.unpack $ UDR.name repository), "'"]


-- Functions/types for internal use.

-- Analyse all commits for the given branch.
{-
    @Issue(
        "Refactor this function into smaller independent functions"
        type="improvement"
        priority="low"
        labels="refactoring"
    )
-}
analyseBranchAll :: USEC.StorageConfig -> T.Text -> UDR.Repository -> T.Text -> IO ()
analyseBranchAll config repositoryId repository branch = do
    -- Get the directory where the repository is located on the disk.
    let directory = UGL.directory repository

    -- Get all commit records for the given branch.
    commitsRecords <- UGCom.getCommits directory branch ["all"]

    {-
        @Issue(
            "Check which of the commits are already analysed so that we don't
            create duplicate commits/issues"
            type="bug"
            priority="low"
            labels="release"
        )
        @Issue(
            "Store all commits' ids in the branch record, as needed for some of
            the timeline graphs"
            type="task"
            priority="low"
            labels="data, release"
        )
    -}

    -- Create the commit records and store them to Elastic Search.
    let fullCommitsRecords = UDEIC.toCommits commitsRecords repositoryId
    indexCommitsResponse <- USEO.bulkIndexDocuments' config (USEC.indexSettingsFromConfig "commit" config) fullCommitsRecords
    {-
        @Issue(
            "Go through the response and log an error if not all Commits where
            properly stored"
            type="bug"
            priority="normal"
            labels="log management"
        )
    -}

    -- Get the tree of files for the commit, filtered by inclusion and exclusion
    -- patterns.

    -- We first need the git tree of files for each commit.
    trees <- mapM (UGT.commitTree' directory) commitsRecords

    --  We check if the tree contains the default configuration file, and we
    -- load it for the commits that it exists. The commits that do not have it
    -- are provided with the default configuration.
    let configsExist = map (\(commit, files) -> (elem UC.defaultConfigFile files, commit)) trees
    configs <- mapM (\(configExists, commit) -> UC.load configExists directory commit) configsExist

    -- We zip the configurations with the commit/tree pairs, and we load the
    -- filtered files based on the file patterns define in the configuration. We
    -- therefore end up with a flat list of files (commit/file pairs) that are
    -- the ones that we will be scanning for Issues.
    let treesWithConfig = zipWith (\config (commit, tree) -> (commit, config, tree)) configs trees
    let files = map UGT.treeGlobFilter' treesWithConfig

    -- Get the contents of all files to be parsed.
    contents <- mapM (UGCon.fileContents' directory) $ concat files

    -- Get all issues for the files' contents.
    let issues = concat $ map UP.parseCommitFileString' contents

    case (length issues) of

        0 -> print "No issues found on any of the commits for this branch."

        _ -> do
            let emRepository = UDER.emptyEmbeddedRepository { UDER._id = repositoryId, UDER.url = (UDR.url repository) }
            let projectIssues = UI.bulkSetRepository issues emRepository
            response <- USEO.bulkIndexIssues config (USEC.indexSettingsFromConfig "issue" config) projectIssues
            {-
                @Issue(
                    "Go through the response and log an error if not all Issues
                    where properly stored"
                    type="bug"
                    priority="normal"
                    labels="log management"
                )
            -}

            return mempty
