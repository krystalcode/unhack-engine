{-# LANGUAGE OverloadedStrings #-}

module Unhack.Pubsub.Repository
       ( analyseAll
       , analyseCommits
       , clone
       , updateHeads) where


-- Imports.

-- External dependencies.

import Control.Monad       (when)
import Data.Aeson          (eitherDecode)
import Data.List.NonEmpty  (fromList)
import Data.Maybe          (fromJust, isJust, isNothing)
import Data.Time           (getCurrentTime)
import Database.Bloodhound
import Network.HTTP.Client

import qualified Data.List       as L  (concat, intersect, lookup, nub, union)
import qualified Data.List.Split as LS (chunksOf)
import qualified Data.Map        as M  (difference, elems, empty, foldl, fromList, insert, keys, lookup, map, mapWithKey, Map)
import qualified Data.Text       as T  (intercalate, pack, unpack, Text)

-- Internal dependencies.

import qualified Unhack.Build.Rule                            as UBR   (apply)
import qualified Unhack.Commit                                as UDC   (emptyCommit, Commit(..))
import qualified Unhack.Config                                as UC    (defaultConfigFile, load)
import qualified Unhack.Data.EmBranch                         as UDEB  (EmBranch(..))
import qualified Unhack.Data.EmCommit                         as UDEC  (fromCommits, EmCommit(..))
import qualified Unhack.Data.EmIssueCommit                    as UDEIC (fromCommits, toCommits, EmIssueCommit(..))
import qualified Unhack.Data.EmbeddedRepository               as UDER
import qualified Unhack.Data.Repository                       as UDR
import qualified Unhack.Git.Branch                            as UGB   (originList)
import qualified Unhack.Git.Commit                            as UGCom (getHeads, list)
import qualified Unhack.Git.Contents                          as UGCon (commitContents)
import qualified Unhack.Git.Fetch                             as UGF   (clone, fetch)
import qualified Unhack.Git.Location                          as UGL   (directory)
import qualified Unhack.Git.Tree                              as UGT   (commitTree', commitTreeLength, treeGlobFilter')
import qualified Unhack.Issue                                 as UI    (bulkSetRepository)
import qualified Unhack.Parser                                as UP    (parseCommitContents)
import qualified Unhack.Pubsub.Publish                        as UPP   (publish)
import qualified Unhack.Storage.ElasticSearch.Config          as USEC  (indexSettingsFromConfig, StorageConfig, StorageIndexSettings)
import qualified Unhack.Storage.ElasticSearch.Data.Branch     as USEDB (updateHeadCommits)
import qualified Unhack.Storage.ElasticSearch.Data.Commit     as USEDC (bulkIndex, bulkMarkProcessed, bulkUpdateBranches, mget)
import qualified Unhack.Storage.ElasticSearch.Data.Repository as USEDR (get, markAccessible, markProcessed, updateHeadCommits)
import qualified Unhack.Storage.ElasticSearch.Operations      as USEO  (bulkIndexDocuments', bulkIndexIssues, search', searchDefaultParams, SearchParams(..))


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

    -- The current time that will be used for the 'createdAt' and 'updatedAt'
    -- fields.
    now <- getCurrentTime

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
            accessibleResponse <- USEDR.markAccessible config indexSettings repositoryId repository now

            -- Send a pubsub message to analyse all commits for the active branches of thies repository.
            pubsubResponse <- UPP.publish [(repositoryId, T.intercalate ":" ["repositories_analyse_all", repositoryId])]

            print $ concat ["Cloned repository '", (T.unpack $ UDR.name repository), "'"]

-- Analyse all commits for the active branches.
analyseAll :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> IO ()
analyseAll storageConfig indexSettings repositoryId = do

    -- The current time that will be used the 'createdAt' and 'updatedAt' fields.
    now <- getCurrentTime

    -- Get the repository record.
    maybeRepository <- USEDR.get storageConfig indexSettings repositoryId

    case (isNothing maybeRepository) of

        True  -> error $ "No repository found with id '" ++ (T.unpack repositoryId) ++ "'."

        False -> do
            let repository          = fromJust maybeRepository
            let directory           = UGL.directory repository
            let maybeActiveBranches = UDR.activeBranches repository

            case isNothing maybeActiveBranches of

              True -> print "Trying to analyse a repository that does not have any active branches."

              False -> do

                  let activeBranches      = fromJust maybeActiveBranches
                  let activeBranchesNames = map UDEB.name activeBranches

                  -- We first make a check that the branches set to be analysed actually exist. The branches that will
                  -- actually be analysed are therefore the intersection of all repository branches with the branches
                  -- set to be analysed.
                  originBranches <- UGB.originList directory
                  let branchesToAnalyseNames      = L.intersect activeBranchesNames originBranches
                  let branchesToAnalyse           = filter (\activeBranch -> UDEB.name activeBranch `elem` branchesToAnalyseNames) activeBranches
                  let mBranchesToAnalyseWithNames = M.fromList $ map (\branch -> (UDEB.name branch, branch)) branchesToAnalyse

                  case (length branchesToAnalyse) of

                    0 -> print "The active branches given do not belong to this repository"

                    _ -> do

                        -- Fetch the branches to analyse from the remote.
                        fetchResponse <- UGF.fetch directory branchesToAnalyseNames

                        -- Get all commit records for the given branches in the form of a Map with the commits' hashes
                        -- as keys and a tuple with the commit's time and the branches it belongs to as the value. We do
                        -- have the commit's ID yet since the commit's information here are fetched from git and the
                        -- commits may have not be stored to Elastic Search yet.
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
                        mCommitsBranchNamesWithHashes <- UGCom.list directory branchesToAnalyseNames
                        let mCommitsMaybeBranchRecordsWithHashes = M.map
                                                                     (\ (time, branches)
                                                                        -> (time, map ((flip M.lookup) mBranchesToAnalyseWithNames) branches))
                                                                     mCommitsBranchNamesWithHashes
                        let mCommitsJustBranchRecordsWithHashes  = M.map
                                                                     (\ (time, branches)
                                                                        -> (time, filter isJust branches))
                                                                     mCommitsMaybeBranchRecordsWithHashes
                        let mCommitsBranchRecordsWithHashes      = M.map
                                                                     (\ (time, branches)
                                                                        -> (time, map fromJust branches))
                                                                     mCommitsJustBranchRecordsWithHashes

                        -- Create a map with the commits for which we already have records. It will be used to update
                        -- the branches for existing records, and to create a map with the commits that are new.
                        let hashes = M.keys mCommitsBranchNamesWithHashes
                        let terms  = TermsQuery "hash" $ fromList hashes
                        let query  = ConstantScoreFilterQuery terms (Boost 1)

                        commitsResponse <- USEO.search' storageConfig (USEC.indexSettingsFromConfig "commit" storageConfig) query (USEO.searchDefaultParams { USEO.spSize = Size (length hashes) })

                        let body         = responseBody commitsResponse
                        let eitherResult = eitherDecode body :: Either String (SearchResult UDC.Commit)
                        let result       = either error id eitherResult
                        let resultHits   = hits . searchHits $ result

                        let lMaybeCommitsWithIds       = map (\hit -> (hitDocId hit, hitSource hit)) resultHits
                        let lJustCommitsWithIds        = filter (\(commitId, commit) -> isJust commit) lMaybeCommitsWithIds
                        let lCommitsWithIds            = map (\(commitId, commit) -> (commitId, fromJust commit)) lJustCommitsWithIds
                        let mExistingCommitsWithHashes = M.fromList $ map (\(commitId, commit) -> (UDC.hash commit, (commitId, commit))) lCommitsWithIds

                        {-
                          @Issue(
                            "Index new and update existing commits in one bulk request"
                            type="improvement"
                            priority="low"
                            labels="performance"
                          )
                        -}

                        -- Create a map with the commits that are new and send them to Elastic Search.
                        let mNewCommitsWithHashes = M.difference mCommitsBranchRecordsWithHashes mExistingCommitsWithHashes
                        let lNewCommits           = M.elems $ M.mapWithKey
                                                                (\ hash (time, branches)
                                                                   -> UDC.emptyCommit { UDC.repositoryId = repositoryId
                                                                                      , UDC.hash         = hash
                                                                                      , UDC.time         = time
                                                                                      , UDC.branches     = Just branches })
                                                                mNewCommitsWithHashes
                        lNewCommitsIds <- USEDC.bulkIndex storageConfig lNewCommits

                        -- Create a map with the commit records that are already stored in Elastic Search, and update
                        -- their branches.
                        let mUpdatedCommitsWithHashes = M.mapWithKey
                                                          (\hash (commitId, commit)
                                                             -> ( commitId
                                                                , commit { UDC.branches = Just (snd $ fromJust (M.lookup hash mCommitsBranchRecordsWithHashes)) }))
                                                          mExistingCommitsWithHashes
                        let mCommitsEmBranchesWithIds = M.foldl
                                                          (\acc (commitId, commit) -> M.insert commitId (UDC.branches commit) acc)
                                                          M.empty
                                                          mUpdatedCommitsWithHashes
                        updateCommitsResponse <- USEDC.bulkUpdateBranches storageConfig mCommitsEmBranchesWithIds now

                        -- Get a list of new commits ids + existing commits ids that are not already processed. These
                        -- will be the commits that we will analyse.
                        let lNonProcessedCommitsWithIds = filter (\(commitId, commit) -> (not . UDC.isProcessed) commit) lCommitsWithIds
                        let lNonProcessedCommitsIds     = map (\(commitId, commit) -> commitId) lNonProcessedCommitsWithIds
                        let lCommitsIdsToAnalyse        = L.union (map DocId lNewCommitsIds) lNonProcessedCommitsIds

                        -- Send pubsub messages to analyse the commits.
                        -- For repositories with large number of files, we want to analyse each commit on a separate task to
                        -- avoid extreme memory consumption and blocking the processing engine instance for long time.
                        -- For small repositories however, we can group processing multiple commits in one run in order to
                        -- avoid the overhead that would be incurred if each commit would be processed as a separate task.
                        -- Based on some experiments, processing 3000 files requires an acceptable amount of time for a single
                        -- task and little memory consumption. We therefore take the number of files in latest commit as a
                        -- sample and group commits together so that each task wouldn't process more than that number of
                        -- files. One commit is the smallest single unit that can be processed, even if a commit would contain
                        -- more files than the limit.
                        {-
                          @Issue(
                            "Get the tree length from the most recent commit instead of the first in the map"
                            type="bug"
                            priority="low"
                          )
                        -}
                        filesCount <- UGT.commitTreeLength directory $ hashes !! 0
                        let nbCommits        = length lCommitsIdsToAnalyse
                        let maxIOOps         = 3000
                        let nbChunks         = ceiling $ fromIntegral (filesCount * nbCommits) / fromIntegral maxIOOps
                        let chunkSize        = ceiling $ fromIntegral nbCommits / fromIntegral nbChunks
                        let commitsIdsChunks = LS.chunksOf chunkSize $ map (T.pack . show) lCommitsIdsToAnalyse
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

                        -- Send a pubsub message to update the head commits for all active branches, and for the repository
                        -- itself as well.
                        {-
                          @Issue(
                            "The updated records for the head commits are not available to search when this task is
                            executed"
                            type="bug"
                            priority="normal"
                            labels="release"
                          )
                        -}
                        pubsubResponse <- UPP.publish [(repositoryId, T.intercalate ":" ["repositories_update_heads", repositoryId])]

                        return mempty

-- Analyse the given commits.
analyseCommits :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> [T.Text] -> IO ()
analyseCommits storageConfig indexSettings repositoryId commitsIds = do

    -- The current time that will be used for the 'createdAt' and 'updatedAt'
    -- fields.
    now <- getCurrentTime

    -- Get the repository record.
    maybeRepository <- USEDR.get storageConfig indexSettings repositoryId

    case (isNothing maybeRepository) of

        True  -> error $ "No repository found with id '" ++ (T.unpack repositoryId) ++ "'."

        False -> do
            let repository  = fromJust maybeRepository
            let directory   = UGL.directory repository
            let lCommitsDocIds = map (read . T.unpack :: T.Text -> DocId) commitsIds
            let lCommitsIds    = map (\(DocId commitId) -> commitId) lCommitsDocIds

            -- Get the commits from Elastic Search.
            maybeCommits <- USEDC.mget storageConfig lCommitsIds

            -- Zip the commits together with their ids as we will be needing them through the process.
            let maybeCommitsWithIds = zipWith (\commitId maybeCommit -> (commitId, maybeCommit)) lCommitsIds maybeCommits

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

                    -- Mark as processed all commits that we parsed for issues.
                    let processedCommitsIds = map (\(commitId, commit) -> commitId) commitsWithIds
                    commitsProcessedResponse <- USEDC.bulkMarkProcessed storageConfig processedCommitsIds now

                    -- Mark the repository as processed if the 'isProcessed' flag is currently set to 'false'. This
                    -- would normally be the case if this is the first time that we are processing the repository.
                    case (UDR.isProcessed repository) of

                        False -> do
                            processedResponse <- USEDR.markProcessed storageConfig indexSettings repositoryId repository now
                            return mempty

                        True  -> return mempty

-- Update the head commits for the Repository and active Branch records for the
-- given repository.
updateHeads :: USEC.StorageConfig -> USEC.StorageIndexSettings -> T.Text -> IO ()
updateHeads storageConfig indexSettings repositoryId = do

    -- The current time that will be used for the 'createdAt' and 'updatedAt'
    -- fields.
    now <- getCurrentTime

    -- Get the repository record.
    maybeRepository <- USEDR.get storageConfig indexSettings repositoryId

    case (isNothing maybeRepository) of

        True  -> error $ "No repository found with id '" ++ (T.unpack repositoryId) ++ "'."

        False -> do

            let repository          = fromJust maybeRepository
            let directory           = UGL.directory repository
            let maybeActiveBranches = UDR.activeBranches repository

            -- We cannot update the heads of neither the repository nor its branches, if we don't have any active
            -- branches. Nothing to do. We shouldn't normally be here in such case, because the user cannot remove all
            -- active branches from the interface, and if the repository does not have active branches to start with
            -- (absence of 'master' branch) an analysis wouldn't be triggered in the first place.
            {-
              @Issue(
                "Log a message instead of throwing an error"
                type="bug"
                priority="low"
                labels="error management, log management"
              )
            -}
            when (isNothing maybeActiveBranches) $ error "Trying to update the heads for a repository that does not have any active branches."

            let activeBranches      = fromJust maybeActiveBranches
            let activeBranchesNames = map UDEB.name activeBranches

            -- Get the heads for the active branches, filtering out any Nothing results. Nothing results should not
            -- normally happen, but somebody could push a master branch before making a commit. In that case, there
            -- is nothing to do.
            maybeHeads <- UGCom.getHeads directory activeBranchesNames
            let branchesWithMaybeHeads  = zipWith (\branch maybeHead -> (branch, maybeHead)) activeBranches maybeHeads
            let branchesWithMaybeHeads' = filter (\(branch, maybeHead) -> isJust maybeHead) branchesWithMaybeHeads
            let branchesWithHeads       = map (\(branch, maybeHead) -> (branch, fromJust maybeHead)) branchesWithMaybeHeads'

            -- Get the commit records for the heads from Elastic Search since we need their ids and other information
            -- that will be embedded in the branch/repository records.
            {-
                @Issue(
                    "Restrict query by repository id filtering"
                    type="improvement"
                    priority="low"
                    labels="performance"
                )
            -}
            let hashes = map (\(branch, head) -> UDEIC.hash head) branchesWithHeads
            let terms  = TermsQuery "hash" $ fromList hashes
            let query  = ConstantScoreFilterQuery terms (Boost 1)

            commitsResponse <- USEO.search' storageConfig (USEC.indexSettingsFromConfig "commit" storageConfig) query USEO.searchDefaultParams

            let body         = responseBody commitsResponse
            let eitherResult = eitherDecode body :: Either String (SearchResult UDC.Commit)
            let result       = either error id eitherResult
            let resultHits   = hits . searchHits $ result

            let maybeCommitsWithIds    = map (\hit -> (hitDocId hit, hitSource hit)) resultHits
            let justCommitsWithIds     = filter (\(commitId, commit) -> isJust commit) maybeCommitsWithIds
            let commitsWithIds         = map (\(commitId, commit) -> (commitId, fromJust commit)) justCommitsWithIds
            let filteredCommitsWithIds = filter (\(commitId, commit) -> repositoryId == UDC.repositoryId commit) commitsWithIds

            let emCommits                  = UDEC.fromCommits filteredCommitsWithIds
            let maybeBranchesWithEmCommits = map (findCommitByHash emCommits) branchesWithHeads
            let justBranchesWithEmCommits  = filter (\branchWithCommit -> isJust branchWithCommit) maybeBranchesWithEmCommits
            let branchesWithEmCommits      = map (\branchWithCommit -> fromJust branchWithCommit) justBranchesWithEmCommits
            let branchesIdsWithEmCommits   = map (\(branch, commit) -> (UDEB._id branch, commit)) branchesWithEmCommits

            updateBranchesResponse <- USEDB.updateHeadCommits storageConfig branchesIdsWithEmCommits now

            -- Update the head commit for the repository as well.
            let defaultBranch = UDR.defaultBranch repository
            let defaultBranchIdWithEmCommit = filter (\(branchId, commit) -> branchId == UDEB._id defaultBranch) branchesIdsWithEmCommits
            let repositoryIdWithEmCommit    = [(repositoryId, snd $ defaultBranchIdWithEmCommit !! 0)]

            updateRepositoryResponse <- USEDR.updateHeadCommits storageConfig repositoryIdWithEmCommit now

            {-
                @Issue(
                    "Update branches and repository records in one query"
                    type="improvement"
                    priority="low"
                    labels="performance"
                )
                @Issue(
                    "Do not update branches and repository records where head commits have not changed"
                    type="improvement"
                    priority="low"
                    labels="performance"
                )
            -}

            return mempty


-- Functions/types for internal use.

findCommitByHash :: [UDEC.EmCommit] -> (UDEB.EmBranch, UDEIC.EmIssueCommit) -> Maybe (UDEB.EmBranch, UDEC.EmCommit)
findCommitByHash [] _ = Nothing
findCommitByHash (commit:xs) (branch, head)
    | UDEC.hash commit == UDEIC.hash head = Just (branch, commit)
    | otherwise                           = findCommitByHash xs (branch, head)
