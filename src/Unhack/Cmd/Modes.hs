{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}

module Unhack.Cmd.Modes
       ( Cmd(..)
       , getCmd)
       where


-- Imports.

import qualified Data.Text as T (Text)
import System.Console.CmdArgs.Implicit
import System.Environment (withArgs)


-- Public API.

-- The following command line handling system has roughly been taken from HLint
-- library.
getCmd :: [String] -> IO Cmd
getCmd args = withArgs (map f args) $ cmdDefaults =<< cmdArgsRun mode
    where f x = if x == "-?" || x == "--help" then "--help=all" else x


-- Functions/types for internal use.

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

{-
    @Issue(
        "Remove the 'git' and 'disk' mode definitions"
        type="task"
        priority="low"
    )
-}
data Cmd
    = CmdGit
        { cmdPath :: FilePath              -- ^ the path to the folder on which to run Unhack, nothing = current working directory
        , cmdRecursive :: Bool             -- ^ whether to run Unhack recursively on all folders below the given path, nothing = True
        , cmdRoot :: Bool                  -- ^ whether to run Unhack on the root folder of the repository, if different than the given path, nothing = True
        , cmdBranch :: T.Text              -- ^ the branch on which to run Unhack on, nothing = the commits will resolve to HEAD
        , cmdCommits :: [T.Text]           -- ^ a list of commits on which to run Unhack on, nothing = the latest commit on the given branch, or the HEAD if no branch
        , cmdProject :: [T.Text]           -- ^ the project identification used when sending the results to a storage engine, nothing = the address of the origin
        , cmdIncludePatterns :: [T.Text]   -- ^ the filename patterns to include, nothing = run on all files
        , cmdExcludePatterns :: [T.Text]   -- ^ the filename patterns to exclude, nothing = do not exclude any files
        , cmdConfigFile :: FilePath        -- ^ the path to the Unhack configuration file to use, nothing = unhack.y(a)ml in the Git root folder
        , cmdOutput :: T.Text              -- ^ where should the result get printed e.g. screen, file, storage, nothing = screen
        , cmdRepositoryId :: T.Text        -- ^ the ID or name of the repository as it will be displayed or stored, nothing = the url of the "origin" remote
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
    | CmdIndexes
        { cmdAction            :: T.Text   -- ^ the action to execute
        , cmdIndex             :: [T.Text] -- ^ the index on which to perform the operation
        , cmdStorageConfigFile :: FilePath -- ^ the path to the storage configuration file, nothing = /etc/unhack/storage.yaml
        }
    | CmdPubSub
        { cmdPubSubConfigFile  :: FilePath -- ^ the path to the pubsub configuration file, nothing = /etc/unhack/pubsub.yaml
        , cmdStorageConfigFile :: FilePath -- ^ the path to the storage configuration file, nothing = /etc/unhack/storage.yaml
        }
    | CmdTest
        { cmdStorageConfigFile :: FilePath -- ^ the path to the storage configuration file, nothing = /etc/unhack/storage.yaml
        , cmdIterations        :: Int      -- ^ the number of test iterations to run
        , cmdRepositories      :: [T.Text] -- ^ the names of the repositories to run the tests on
        }
    deriving (Data, Typeable, Show)

mode = cmdArgsMode $ modes
    [ CmdGit
        { cmdPath = name' "path" &= typ "FILE/DIR" &= help "The path to run Unhack on - defaults to the current working directory."
        , cmdRecursive = True &= explicit &= name "recursive" &= help "Whether to run recursively on all folders & files below the given path - defaults to TRUE"
        , cmdRoot = True &= explicit &= name "root" &= help "Whether to run from the root folder of the repository in which the given path belongs - defaults to TRUE"
        , cmdBranch = name' "branch" &= typ "BRANCH" &= help "The branch the latest commit of which to run on - defaults to no branch and the commits will resolve to HEAD"
        , cmdCommits = name' "commits" &= typ "HASH" &= help "A list of commits to run on, or \"all\" to run on all commits - defaults to the latest commit on the given branch, or to HEAD if no branch is given"
        , cmdProject = name' "project" &= typ "PROJECT_ID" &= help "The project identification name or number to use when sending the results to a storage engint - defaults to the address of the origin"
        , cmdIncludePatterns = name' "include" &= typ "PATTERN" &= help "A list of filename patterns to include - defaults to including all files"
        , cmdExcludePatterns = name' "exclude" &= typ "PATTERN" &= help "A list of filename patterns to exclude - defaults to not exclude any files"
        , cmdConfigFile = name' "config-file" &= typFile &= help "The path to the Unhack configuration to use - defaults unhack.y(a)ml in the root folder of the repository"
        , cmdOutput = name'' "output" "screen" &= typ "OUTPUT" &= help "Where to output the results (screen, file, storage)"
        , cmdRepositoryId = name' "repository-id" &= typ "REPOSITORY_ID" &= help "The ID or name of the repository for display/storage purposes - defaults to the url of the \"origin\" remote"
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
    , CmdIndexes
        { cmdAction = name' "action" &=typ "ACTION" &= help "The action to perform. Available actions are \"create_index\", \"delete_index\", \"put_mapping\", \"delete_mapping\", \"create_index_with_mapping\", \"recreate_index\", \"recreate_index_with_mapping\"."
        , cmdIndex = name' "index" &=typ "INDEX" &= help "The index on which to perform the operation. 'all' will result in the operation being executed on all indexes."
        , cmdStorageConfigFile = name'' "storage-config-file" "/etc/unhack/storage.yaml" &= typFile &= help "The path to the file with the storage configuration - defaults to /etc/unhack/storage.yaml"
        } &= explicit &= name "indexes"
    , CmdPubSub
        { cmdPubSubConfigFile = name'' "pubsub-config-file" "/etc/unhack/pubsub.yaml" &= typFile &= help "The path to the file with the pubsub configuration - defaults to /etc/unhack/pubsub.yaml"
        , cmdStorageConfigFile = name'' "storage-config-file" "/etc/unhack/storage.yaml" &= typFile &= help "The path to the file with the storage configuration - defaults to /etc/unhack/storage.yaml"
        } &= explicit &= name "pubsub"
    , CmdTest
        { cmdStorageConfigFile = name'' "storage-config-file" "/etc/unhack/storage.yaml" &= typFile &= help "The path to the file with the storage configuration - defaults to /etc/unhack/storage.yaml"
        , cmdIterations = name'' "iterations" 1 &= help "The number of test iterations to run"
        , cmdRepositories = name' "repository" &=typ "REPOSITORY" &= help "The names of the repositories to run the tests on"
        } &= explicit &= name "test"
    ] &= program "unhack" &= verbosity
    &=  summary ("Unhack v0.1.0.0, (C) Dimitris Bozelos 2015-2016")
    where
        name' optionName = def &= explicit &= name optionName
        name'' optionName defaultValue = defaultValue &= explicit &= name optionName

instance Default T.Text where def = ""
