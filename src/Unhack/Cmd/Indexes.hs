{-# LANGUAGE OverloadedStrings #-}

module Unhack.Cmd.Indexes
       ( runIndexes
       ) where


-- Imports.

import           Unhack.Cmd.Modes
import qualified Unhack.Storage.ElasticSearch.Config     as USEC (load)
import qualified Unhack.Storage.ElasticSearch.Operations as USEO (createIndexes, deleteIndexes, deleteMappings, putMappings)


-- Public API.
runIndexes :: Cmd -> IO ()
runIndexes cmd = do

    -- Get storage configuration.
    let storageConfigFile = cmdStorageConfigFile cmd

    -- Get options values.
    let action  = cmdAction cmd
    let indexes = cmdIndex cmd

    -- Get the storage engine configuration.
    storageConfig <- USEC.load storageConfigFile

    case action of "create" -> do
                                     putStrLn "Creating the Elastic Search index ..."
                                     indexResponse <- USEO.createIndexes storageConfig indexes
                                     print indexResponse
                   "delete" -> do
                                     putStrLn "Deleting the Elastic Search index ..."
                                     indexResponse <- USEO.deleteIndexes storageConfig indexes
                                     print indexResponse
                   "put_mappings"    -> do
                                     putStrLn "Creating the mapping for the Elastic Search index ..."
                                     mappingResponse <- USEO.putMappings storageConfig indexes
                                     print mappingResponse
                   "delete_mappings" -> do
                                     putStrLn "Deleting the mapping for the Elastic Search index ..."
                                     mappingResponse <- USEO.deleteMappings storageConfig indexes
                                     print mappingResponse
                   "create_with_mappings" -> do
                                     putStrLn "Creating the Elastic Search index ..."
                                     indexResponse <- USEO.createIndexes storageConfig indexes
                                     print indexResponse
                                     putStrLn "Creating the mapping for the Elastic Search index ..."
                                     mappingResponse <- USEO.putMappings storageConfig indexes
                                     print mappingResponse
                   "recreate" -> do
                                     putStrLn "Deleting the Elastic Search index ..."
                                     dIndexResponse <- USEO.deleteIndexes storageConfig indexes
                                     print dIndexResponse
                                     putStrLn "Creating the Elastic Search index ..."
                                     cIndexResponse <- USEO.createIndexes storageConfig indexes
                                     print cIndexResponse
                   "recreate_with_mappings" -> do
                                     putStrLn "Deleting the Elastic Search index ..."
                                     dIndexResponse <- USEO.deleteIndexes storageConfig indexes
                                     print dIndexResponse
                                     putStrLn "Creating the Elastic Search index ..."
                                     cIndexResponse <- USEO.createIndexes storageConfig indexes
                                     print cIndexResponse
                                     putStrLn "Creating the mapping for the Elastic Search index ..."
                                     mappingResponse <- USEO.putMappings storageConfig indexes
                                     print mappingResponse

                   "" -> error "You must specify an action to perform."

                   _ -> error "The action you have specified is not recognised."
