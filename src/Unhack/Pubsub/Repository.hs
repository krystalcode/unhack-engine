{-# LANGUAGE OverloadedStrings #-}

module Unhack.Pubsub.Repository
       ( analyseAll
       , analyseCommits
       , clone ) where


-- Imports.

-- External dependencies.

import Data.Aeson          (eitherDecode)
import Data.Maybe          (fromJust, isJust, isNothing)
import Database.Bloodhound
import Network.HTTP.Client

import qualified Data.List       as L  (intersect, lookup, nub, concat)
import qualified Data.List.Split as LS (chunksOf)
import qualified Data.Text       as T  (intercalate, unpack, Text)

-- Internal dependencies.

import qualified Unhack.Build.Rule                            as UBR   (apply)
import qualified Unhack.Commit                                as UDC   (Commit(..))
import qualified Unhack.Config                                as UC    (defaultConfigFile, load)
import qualified Unhack.Data.EmBranch                         as UDEB
import qualified Unhack.Data.EmIssueCommit                    as UDEIC (fromCommits, toCommits, EmIssueCommit(..))
import qualified Unhack.Data.EmbeddedRepository               as UDER
import qualified Unhack.Data.Repository                       as UDR
import qualified Unhack.Git.Branch                            as UGB   (branchesList)
import qualified Unhack.Git.Commit                            as UGCom (getCommits)
import qualified Unhack.Git.Contents                          as UGCon (commitContents)
import qualified Unhack.Git.Fetch                             as UGF   (clone)
import qualified Unhack.Git.Location                          as UGL   (directory)
import qualified Unhack.Git.Tree                              as UGT   (commitTree', commitTreeLength, treeGlobFilter')
import qualified Unhack.Issue                                 as UI    (bulkSetRepository)
import qualified Unhack.Parser                                as UP    (parseCommitContents)
import qualified Unhack.Pubsub.Publish                        as UPP   (publish)
import qualified Unhack.Storage.ElasticSearch.Config          as USEC  (indexSettingsFromConfig, StorageConfig, StorageIndexSettings)
import qualified Unhack.Storage.ElasticSearch.Data.Branch     as USEDB (updateCommitsIds)
import qualified Unhack.Storage.ElasticSearch.Data.Commit     as USEDC (mget)
import qualified Unhack.Storage.ElasticSearch.Data.Repository as USEDR (get, markAccessible, markProcessed)
import qualified Unhack.Storage.ElasticSearch.Operations      as USEO  (bulkIndexDocuments', bulkIndexIssues)


-- Public API.
{-
    @Issue(
        "Refactor all tasks into smaller, comprehensive functions"
        type="task"
        priority="low"
        labels="readability, refactoring"
    )
-}

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

            -- Send a pubsub message to analyse all commits for the active branches of thies repository.
            pubsubResponse <- UPP.publish [(repositoryId, T.intercalate ":" ["repositories_analyse_all", repositoryId])]

            print $ concat ["Cloned repository '", (T.unpack $ UDR.name repository), "'"]

-- Analyse all commits for the active branches.
analyseAll :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> IO ()
analyseAll storageConfig indexSettings repositoryId = do

    -- Get the repository record.
    maybeRepository <- USEDR.get storageConfig indexSettings repositoryId

    case (isNothing maybeRepository) of

        True  -> error $ "No repository found with id '" ++ (T.unpack repositoryId) ++ "'."

        False -> do
            let repository          = fromJust maybeRepository
            let directory           = UGL.directory repository
            let activeBranches      = UDR.activeBranches repository
            let activeBranchesNames = map UDEB.name activeBranches

            case (length activeBranches) of

              0 -> print "Trying to analyse a repository that does not have any active branches."

              _ -> do
                  -- We first make a check that the branches set to be analysed actually exist. The branches that will
                  -- actually be analysed are therefore the intersection of all repository branches with the branches
                  -- set to be analysed.
                  branchesList <- UGB.branchesList directory
                  let branchesToAnalyseNames = L.intersect activeBranchesNames branchesList
                  let branchesToAnalyse      = filter (\activeBranch -> UDEB.name activeBranch `elem` branchesToAnalyseNames) activeBranches

                  {-
                      @Issue(
                          "Terminate execution if there are no branches to analyse"
                          type="bug"
                          priority="low"
                          labels="release"
                      )
                  -}

                  -- Get all commit records for the given branches. These are are EmIssueCommit records created for all
                  -- commit hashes/times fetched by git, and they do not contain an _id since they are not stored to
                  -- Elastic Search yet.
                  {-
                      @Issue(
                          "Create an 'analyse_recent' task that will analyse only N recent commits, where N maybe
                          different per repository"
                          type="improvement"
                          priority="low"
                          labels="performance, release"
                      )
                      @Issue(
                          "Log number of commits analysed by the 'analyse_recent' task in order to find a safe number of
                          recent commits per repository"
                          type="improvement"
                          priority="low"
                          labels="analytics, release"
                      )
                      @Issue(
                          "Consider detecting commits not processed via cron job, in case they escape the
                          'analyse_recent' task"
                          type="bug"
                          priority="low"
                          labels="release"
                      )
                  -}
                  commitsRecords <- mapM (UGCom.getCommits directory ["all"]) branchesToAnalyseNames

                  -- Create a flat list of commits with duplicates removed. Duplicates can occur because one commit may
                  -- be part of many branches.
                  let commitsRecordsUnique = L.nub $ L.concat commitsRecords

                  {-
                      @Issue(
                          "Check which of the commits are already analysed so that we don't
                          create duplicate commits/issues"
                          type="bug"
                          priority="low"
                          labels="release"
                      )
                  -}

                  -- Create the commit records and store them to Elastic Search.
                  let fullCommitsRecords = UDEIC.toCommits repositoryId commitsRecordsUnique
                  indexCommitsResponse <- USEO.bulkIndexDocuments' storageConfig (USEC.indexSettingsFromConfig "commit" storageConfig) fullCommitsRecords

                  {-
                      @Issue(
                          "Go through the response and log an error if not all Commits where
                          properly stored"
                          type="bug"
                          priority="normal"
                          labels="log management"
                      )
                  -}

                  -- Get the ids of the commits from the response and create a list of 'EmIssueCommit' records that contain those ids.
                  let body                  = responseBody indexCommitsResponse
                  let eitherResult          = eitherDecode body :: Either String BulkEsResult
                  let result                = either error id eitherResult
                  let resultItems           = berItems result
                  let resultItemsInner      = map bericCreate resultItems
                  let commitsIds            = map berId resultItemsInner

                  -- Map the commits ids returned in the response to their branches.
                  let branchesIdsWithCommitsHashes = zipWith (\emBranch branchEmCommits -> (UDEB._id emBranch, map UDEIC.hash branchEmCommits)) branchesToAnalyse commitsRecords
                  let uniqueCommitsHashesWithIds   = zipWith (\emCommit commitId -> (UDEIC.hash emCommit, commitId)) commitsRecordsUnique commitsIds
                  let branchesIdsWithCommitsIds    = map (\(branchId, hashes) -> (branchId, map (fromJust <$> (flip L.lookup) uniqueCommitsHashesWithIds) hashes)) branchesIdsWithCommitsHashes

                  {- @Issue( "Handle errors"
                             type="bug"
                             priority="low"
                             labels="error management, log management" ) -}
                  bulkUpdateResponse <- USEDB.updateCommitsIds storageConfig branchesIdsWithCommitsIds

                  -- Send pubsub messages to analyse the commits.
                  -- For repositories with large number of files, we want to analyse each commit on a separate task to
                  -- avoid extreme memory consumption and blocking the processing engine instance for long time.
                  -- For small repositories however, we can group processing multiple commits at one run in order to
                  -- avoid the overhead that would be incurred if each commit would be processed as a separate task.
                  -- Based on some experiments, processing 3000 files requires an acceptable amount of time for a single
                  -- task and little memory consumption. We therefore take the number of files in latest commit as a
                  -- sample and group commits together so that each task wouldn't process more than that number of
                  -- files. One commit is the smallest single unit that can be processed, even if a commit would contain
                  -- more files than the limit.
                  filesCount <- UGT.commitTreeLength directory $ commitsRecordsUnique !! 0
                  let nbCommits        = length commitsIds
                  let maxIOOps         = 3000
                  let nbChunks         = ceiling $ fromIntegral (filesCount * nbCommits) / fromIntegral maxIOOps
                  let chunkSize        = ceiling $ fromIntegral nbCommits / fromIntegral nbChunks
                  let commitsIdsChunks = LS.chunksOf chunkSize commitsIds
                  let messagesPrefix   = ["repositories_analyse_commits", repositoryId]
                  let messages         = map (\commitsIdsChunk -> (repositoryId, T.intercalate ":" $ (++) messagesPrefix commitsIdsChunk)) commitsIdsChunks

                  {- @Issue( "Handle errors"
                             type="bug"
                             priority="low"
                             labels="error management, log management" )
                     @Issue( "Investigate a solution for better distribution of worker resources between repositories
                             that run on the same engine instance"
                             type="bug"
                             priority="low"
                             labels="ux" )-}
                  pubsubResponse <- UPP.publish messages

                  return mempty

-- Analyse the given commits.
analyseCommits :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> [T.Text] -> IO ()
analyseCommits storageConfig indexSettings repositoryId commitsIds = do

    -- Get the repository record.
    maybeRepository <- USEDR.get storageConfig indexSettings repositoryId

    case (isNothing maybeRepository) of

        True  -> error $ "No repository found with id '" ++ (T.unpack repositoryId) ++ "'."

        False -> do
            let repository = fromJust maybeRepository
            let directory  = UGL.directory repository

            -- Get the commits from Elastic Search.
            maybeCommits <- USEDC.mget storageConfig commitsIds

            -- Zip the commits together with their ids as we will be needing them through the process.
            let maybeCommitsWithIds = zipWith (\commitId maybeCommit -> (commitId, maybeCommit)) commitsIds maybeCommits

            -- Filter out commits not found and unwrap them from Maybes.
            let foundMaybeCommitsWithIds = filter (\(commitId, maybeCommit) -> isJust maybeCommit) maybeCommitsWithIds
            let foundCommitsWithIds      = map (\(commitId, maybeCommit) -> (commitId, fromJust maybeCommit)) foundMaybeCommitsWithIds

            -- Filter out commits that have already been processed.
            let commitsWithIds = filter (\(commitId, commit) -> not . UDC.isProcessed $ commit) foundCommitsWithIds

            -- Get the 'EmIssueCommit' records that we will work with for the rest of the algorithm, and that we will
            -- embed in the 'Issue' records.
            let emCommits = UDEIC.fromCommits commitsWithIds

            -- We first need the git tree of files for each commit.
            trees <- mapM (UGT.commitTree' directory) emCommits

            -- We check if the tree contains the default configuration file, and we load it for the commits that it
            -- exists. The commits that do not have it are provided with the default configuration.
            let repoConfigsExist = map (\(commit, files) -> (elem UC.defaultConfigFile files, commit)) trees
            repoConfigs <- mapM (\(repoConfigExists, commit) -> UC.load repoConfigExists directory commit) repoConfigsExist

            -- We zip the configurations with the commit/tree pairs, and we load the filtered files based on the file
            -- patterns define in the configuration. We therefore end up with a flat list of files (commit/file pairs)
            -- that are the ones that we will be scanning for Issues.
            let treesWithRepoConfig = zipWith (\repoConfig (commit, tree) -> (commit, repoConfig, tree)) repoConfigs trees
            let files = map UGT.treeGlobFilter' treesWithRepoConfig

            -- Get the contents of all files to be parsed.
            contents <- mapM (UGCon.commitContents directory) files

            -- Get all issues for the files' contents.
            let issues = map UP.parseCommitContents contents

            -- Apply rules define in the repository configuration per commit.
            {-
                @Issue(
                    "Support a default configuration per project/repository provided by user through the web UI"
                    type="feature"
                    priority="low"
                )
                @Issue(
                    "Support constructing a build message together with the build status"
                    type="bug"
                    priority="high"
                )
                @Issue(
                    "Support storing log messages that would be used to display in the web UI any errors
                    that occurred during the build"
                    type="feature"
                    priority="normal"
                )
                @Issue(
                    "Update the branch and the repository with the head commit and build results"
                    type="bug"
                    priority="high"
                )
            -}
            let issuesWithRepoConfig = zipWith (\repoConfig (commit, commitIssues) -> (commit, repoConfig, commitIssues)) repoConfigs issues
            rulesResult <- mapM_ (\(commit, repoConfig, commitIssues) -> UBR.apply storageConfig repoConfig commit commitIssues) issuesWithRepoConfig

            -- If there are issues found, send them to Elastic Search.
            case (length issues) of

                0 -> print "No issues found on any of the commits for this branch."

                _ -> do
                    let emRepository  = UDER.emptyEmbeddedRepository { UDER._id = repositoryId, UDER.url = (UDR.url repository) }
                    let flatIssues    = concat $ map (\(commit, commitIssues) -> commitIssues) issues
                    let projectIssues = UI.bulkSetRepository flatIssues emRepository
                    response <- USEO.bulkIndexIssues storageConfig (USEC.indexSettingsFromConfig "issue" storageConfig) projectIssues
                    --print "Stored issues ..."
                    {-
                        @Issue(
                            "Go through the response and log an error if not all Issues
                            where properly stored"
                            type="bug"
                            priority="normal"
                            labels="log management"
                        )
                    -}

                    -- Mark the repository as processed if the 'isProcessed' flag is currently set to 'false'. This
                    -- would normally be the case if this is the first time that we are processing the repository.
                    case (UDR.isProcessed repository) of

                        False -> do
                            processedResponse <- USEDR.markProcessed storageConfig indexSettings repositoryId repository
                            return mempty

                        True  -> return mempty


-- Functions/types for internal use.
