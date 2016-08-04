{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}

module Unhack.Cmd.Modes
       ( Cmd(..)
       , getCmd)
       where


-- Imports.

import System.Console.CmdArgs.Implicit
import System.Environment              (withArgs)

import qualified Data.Text as T (Text)


-- Public API.

-- The following command line handling system has roughly been taken from HLint
-- library.
getCmd :: [String] -> IO Cmd
getCmd args = withArgs (map f args) $ cmdDefaults =<< cmdArgsRun mode
    where f x = if x == "-?" || x == "--help" then "--help=all" else x


-- Functions/types for internal use.

-- Set some defaults when no argument has been given.
cmdDefaults :: Cmd -> IO Cmd
cmdDefaults x = return x

data Cmd
    = CmdIndexes
        { cmdAction            :: T.Text   -- ^ the action to execute
        , cmdIndex             :: [T.Text] -- ^ the index on which to perform the operation
        , cmdStorageConfigFile :: FilePath -- ^ the path to the storage configuration file, nothing = /etc/unhack/storage.yaml
        }
    | CmdPubSub
        { cmdPubSubConfigFile  :: FilePath -- ^ the path to the pubsub configuration file, nothing = /etc/unhack/pubsub.yaml
        , cmdStorageConfigFile :: FilePath -- ^ the path to the storage configuration file, nothing = /etc/unhack/storage.yaml
        }
    deriving (Data, Typeable, Show)

mode = cmdArgsMode $ modes
    [ CmdIndexes
        { cmdAction = name' "action" &=typ "ACTION" &= help "The action to perform. Available actions are \"create_index\", \"delete_index\", \"put_mapping\", \"delete_mapping\", \"create_index_with_mapping\", \"recreate_index\", \"recreate_index_with_mapping\"."
        , cmdIndex = name' "index" &=typ "INDEX" &= help "The index on which to perform the operation. 'all' will result in the operation being executed on all indexes."
        , cmdStorageConfigFile = name'' "storage-config-file" "/etc/unhack/storage.yaml" &= typFile &= help "The path to the file with the storage configuration - defaults to /etc/unhack/storage.yaml"
        } &= explicit &= name "indexes"
    , CmdPubSub
        { cmdPubSubConfigFile = name'' "pubsub-config-file" "/etc/unhack/pubsub.yaml" &= typFile &= help "The path to the file with the pubsub configuration - defaults to /etc/unhack/pubsub.yaml"
        , cmdStorageConfigFile = name'' "storage-config-file" "/etc/unhack/storage.yaml" &= typFile &= help "The path to the file with the storage configuration - defaults to /etc/unhack/storage.yaml"
        } &= explicit &= name "pubsub"
    ] &= program "unhack" &= verbosity
    &=  summary ("Unhack v0.1.0.0, (C) Dimitris Bozelos 2015-2016")
    where
        name' optionName = def &= explicit &= name optionName
        name'' optionName defaultValue = defaultValue &= explicit &= name optionName

instance Default T.Text where def = ""
