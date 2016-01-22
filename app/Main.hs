{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}

module Main where

import qualified Data.Text as T (unpack, Text)
import System.Console.CmdArgs.Implicit
import System.Environment (getArgs, withArgs)
import Unhack.Commit
import Unhack.Git.Commit
import Unhack.Git.Contents
import Unhack.Git.Tree
import Unhack.Issue
import Unhack.Parser
import qualified Unhack.Storage.ElasticSearch.Config as USC

{-
    @Issue(
        "Add error handling in command line"
        type="bug"
        priority="low"
        labels="ux"
    )
    @Issue(
        "Colorise command line output"
        type="improvement"
        priority="low"
        labels="ux"
    )
    @Issue(
        "Add configuration options for defining colors per property"
        type="feature"
        priority="low"
        labels="ux"
    )
-}

main = do
    args <- getArgs
    cmd <- getCmd args
    case cmd of
        CmdGit{} -> runGit cmd
        CmdDisk{} -> runDisk cmd

{-
    @Issue(
        "Move command line handling into a separate module"
        type="task"
        priority="low"
        labels="structure"
    )
    @Issue(
        "Add verbosity option and print progress statements"
        type="feature"
        priority="low"
        labels="ux"
    )
    @Issue(
        "Add logging"
        type="feature"
        priority="normal"
        labels="log management"
    )
    @Issue(
        "Add filtering options"
        type="feature"
        priority="normal"
    )
    @Issue(
        "Add statistics command e.g. available labels, number of issues
        filtered by type or priority etc."
        type="feature"
        priority="low"
        labels="editor integration"
    )
    @Issue(
        "Add statistics command e.g. available labels, number of issues
        filtered by type or priority etc."
        type="feature"
        priority="low"
        labels="editor integration"
    )
    @Issue(
        "Convert modes to process based [issues, statistics] and add source
        [git, filesystem] and output [shell, json, storage] options"
        type="improvement"
        priority="normal"
        labels="modularity"
    )
-}

runGit :: Cmd -> IO ()
runGit cmd = do
    -- Get options values.
    let directory = cmdPath cmd
    let branch = cmdBranch cmd
    let includePatterns = cmdIncludePatterns cmd
    let excludePatterns = cmdExcludePatterns cmd
    let output = cmdOutput cmd

    case (output) of "storage" -> do
                                  storageConfig <- USC.load $ cmdStorageConfigFile cmd
                                  let storageType = USC.type' storageConfig
                                  let storageHost = USC.host storageConfig
                                  let storagePort = USC.port storageConfig
                                  putStrLn $ "Setting the output to be sent to a storage engine of type \"" ++ (T.unpack storageType) ++ "\" ..."
                     "screen" -> print "Setting the output to be printed on the screen ..."
                     _ -> print $ "Setting the output to be written on the file \"" ++ (T.unpack output) ++ "\""

    {-
        @Issue(
            "Implement glob pattern matching and remove valid extensions
            configuration"
            type="feature"
            priority="normal"
        )
    -}
    let validExtensions = ["hs", "yaml", "yml"]

    {-
        @Issue(
            "Implement loading configuration options from file for options not
            defined in command"
            type="feature"
            priority="normal"
        )
    -}

    -- Get the latest commit for the specified branch.
    {-
        @Issue(
            "Implement using the currently checked out branch/commit if no
            branch or commit are specified"
            type="feature"
            priority="normal"
        )
    -}
    commitsText <- logCommitsText directory branch 1
    let commits = logTextToCommits commitsText

    -- Get the tree of files for the commit, filtered by inclusion and
    -- exclusion patterns.
    trees <- mapM (commitTree' directory) commits
    let treesInText = map (\x@(commit, tree) -> (commit, tree)) trees
    let files = mapM (treeValidFiles' includePatterns excludePatterns validExtensions) treesInText

    -- Get the contents of all files to be parsed.
    contents <- mapM (fileContents' directory) $ concat files

    -- Get all issues for the files' contents.
    let issues = concat $ map parseCommitFileString' contents

    -- Print out all issues.
    putStr (unlines . map displayIssue $ issues)

runDisk :: Cmd -> IO ()
runDisk cmd = print "Running the disk command."

-- The following command line handling system has roughly been taken from HLint
-- library.
getCmd :: [String] -> IO Cmd
getCmd args = withArgs (map f args) $ cmdDefaults =<< cmdArgsRun mode
    where f x = if x == "-?" || x == "--help" then "--help=all" else x

-- Set some defaults when no argument has been given.
{-
    @Issue(
        "Could these default be set directly instead using def?"
        type="task"
        priority="low"
    )
-}
cmdDefaults :: Cmd -> IO Cmd
cmdDefaults CmdGit{..} = do
    cmdPath <- return $ if null cmdPath then "." else cmdPath
    return CmdGit{..}
cmdDefaults CmdDisk{..} = do
    cmdPath <- return $ if null cmdPath then "." else cmdPath
    return CmdDisk{..}
cmdDefaults x = return x

data Cmd
    = CmdGit
        { cmdPath :: FilePath              -- ^ the path to the folder on which to run Unhack, nothing = current working directory
        , cmdRecursive :: Bool             -- ^ whether to run Unhack recursively on all folders below the given path, nothing = True
        , cmdRoot :: Bool                  -- ^ whether to run Unhack on the root folder of the repository, if different than the given path, nothing = True
        , cmdBranch :: T.Text              -- ^ the branch on which to run Unhack on, nothing = the currently checked out branch, if any
        , cmdCommit :: T.Text              -- ^ the commit on which to run Unhack on, nothing = the latest commit on the checked out branch, if any
        , cmdProject :: [T.Text]           -- ^ the project identification used when sending the results to a storage engine, nothing = the address of the origin
        , cmdIncludePatterns :: [T.Text]   -- ^ the filename patterns to include, nothing = run on all files
        , cmdExcludePatterns :: [T.Text]   -- ^ the filename patterns to exclude, nothing = do not exclude any files
        , cmdConfigFile :: FilePath        -- ^ the path to the Unhack configuration file to use, nothing = unhack.y(a)ml in the Git root folder
        , cmdOutput :: T.Text              -- ^ where should the result get printed e.g. screen, file, storage, nothing = screen
        , cmdStorageType :: T.Text         -- ^ the type of storage to send the results to
        , cmdStorageHost :: T.Text         -- ^ the host of the storage engine
        , cmdStoragePort :: Int            -- ^ the port that the storage engine listens to
        , cmdStorageConfigFile :: FilePath -- ^ the path to the storage configuration file, nothing = /etc/unhack/storage.yaml
        }
    | CmdDisk
        { cmdPath :: FilePath              -- ^ the path to the folder on which to run Unhack, nothing = current working directory
        , cmdRecursive :: Bool             -- ^ whether to run Unhack recursively on all folders below the given path, nothing = True
        , cmdConfigFile :: FilePath        -- ^ the path to the Unhack configuration file to use, nothing = unhack.y(a)ml in the given path
        , cmdIncludePatterns :: [T.Text]   -- ^ the filename patterns to include, nothing = run on all files
        , cmdExcludePatterns :: [T.Text]   -- ^ the filename patterns to exclude, nothing = do not exclude any files
        }
    deriving (Data, Typeable, Show)

mode = cmdArgsMode $ modes
    [ CmdGit
        { cmdPath = name' "path" &= typ "FILE/DIR" &= help "The path to run Unhack on - defaults to the current working directory."
        , cmdRecursive = True &= explicit &= name "recursive" &= help "Whether to run recursively on all folders & files below the given path - defaults to TRUE"
        , cmdRoot = True &= explicit &= name "root" &= help "Whether to run from the root folder of the repository in which the given path belongs - defaults to TRUE"
        , cmdBranch = name' "branch" &= typ "BRANCH" &= help "The branch the latest commit of which to run on - defaults to the currently checked out branch"
        , cmdCommit = name' "commit" &= typ "HASH" &= help "The specific commit to run on - defaults to the latest commit on the checked out branch"
        , cmdProject = name' "project" &= typ "PROJECT_ID" &= help "The project identification name or number to use when sending the results to a storage engint - defaults to the address of the origin"
        , cmdIncludePatterns = name' "include" &= typ "PATTERN" &= help "A list of filename patterns to include - defaults to including all files"
        , cmdExcludePatterns = name' "exclude" &= typ "PATTERN" &= help "A list of filename patterns to exclude - defaults to not exclude any files"
        , cmdConfigFile = name' "config-file" &= typFile &= help "The path to the Unhack configuration to use - defaults unhack.y(a)ml in the root folder of the repository"
        , cmdOutput = name'' "output" "screen" &= typ "OUTPUT" &= help "Where to output the results (screen, file, storage)"
        , cmdStorageType = name' "storage-type" &= typ "TYPE" &= help "The type of the storage engine used. Supported types are 'elasticsearch'."
        , cmdStorageHost = name' "storage-host" &= typ "HOST" &= help "The host of the storage engine."
        , cmdStoragePort = name' "storage-port" &= typ "PORT" &= help "The port that the storage engine is listening to."
        , cmdStorageConfigFile = name'' "storage-config-file" "/etc/unhack/storage.yaml" &= typFile &= help "The path to the file with the storage configuration - defaults to /etc/unhack/storage.yaml"
        } &= auto &= explicit &= name "git"
    , CmdDisk
        { cmdPath = name' "path" &= typ "FILE/DIR" &= help "The path to run Unhack on - defaults to the current working directory."
        , cmdRecursive = name' "recursive" &= help "Whether to run recursively on all folders & files below the given path - defaults to TRUE"
        , cmdConfigFile = name' "config-file" &= typFile &= help "The path to the Unhack configuration to use - defaults unhack.y(a)ml in the root folder of the repository"
        , cmdIncludePatterns = name' "config-include" &= typ "PATTERN" &= help "A list of filename patterns to include - defaults to including all files"
        , cmdExcludePatterns = name' "config-exclude" &= typ "PATTERN" &= help "A list of filename patterns to exclude - defaults to not exclude any files"
        } &= explicit &= name "disk"
    ] &= program "unhack" &= verbosity
    &=  summary ("Unhack v0.1.0.0, (C) Dimitris Bozelos 2015-2016")
    where
        name' optionName = def &= explicit &= name optionName
        name'' optionName defaultValue = defaultValue &= explicit &= name optionName

instance Default T.Text where def = ""
