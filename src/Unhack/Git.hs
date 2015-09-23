module Unhack.Git
       ( listCommitsIO
       , listCommits
       , lsTreeIO
       , unlinesTree
       , showFileIO
       , parseTrees
       ) where

import Unhack.ElasticSearch
import Unhack.Parser
import Unhack.Issue

import System.IO
import System.Process
import System.Exit
import Data.List
import Data.List.Split

-- Public API.

{-
  @Issue(
    "Use git log --pretty="format:%H" to avoid grep and filtering"
    type="improvement"
    priority="low"
  )
-}
listCommitsIO :: FilePath -> IO (String)
listCommitsIO directory = do
    (_, Just hout, Just herr, procHandle) <- createProcess $ createCommand "git log | grep commit" directory
    exitCode <- waitForProcess procHandle
    stdOut   <- hGetContents hout
    stdErr   <- hGetContents herr
    if exitCode == ExitSuccess
        then return stdOut
        else error $ stdErr ++ stdOut

listCommits :: String -> [String]
listCommits [] = []
listCommits input = map (drop 7) filteredLines
    where lines = splitOn "\n" input
          -- Keep only commit lines. There may be message lines that
          -- contain the word "commit" as well.
          filteredLines = filter (isPrefixOf "commit ") lines

-- Get the contents of a file on a specific commit.
showFileIO :: String -> (String, String) -> IO (String, String, String)
showFileIO directory (commit, filepath) = do
  (_, Just hout, Just herr, procHandle) <- createProcess $ createCommand command directory
  exitCode <- waitForProcess procHandle
  stdOut   <- hGetContents hout
  stdErr   <- hGetContents herr
  if exitCode == ExitSuccess
     then return (commit, filepath, stdOut)
     -- Continue in the case of an error, such as when the file does not exist in the commit.
     else return (commit, filepath, "")
  where command = "git show " ++ commit ++ ":" ++ filepath

-- Get the full git tree on a specific commit.
lsTreeIO :: FilePath -> String -> IO (String, String)
lsTreeIO directory commit = do
    (_, Just hout, Just herr, procHandle) <- createProcess $ createCommand ("git ls-tree --full-tree --name-only -r " ++ commit) directory
    exitCode <- waitForProcess procHandle
    stdOut   <- hGetContents hout
    stdErr   <- hGetContents herr
    if exitCode == ExitSuccess
        then return (commit, stdOut)
        else error $ stdErr ++ stdOut

-- Convert a git tree (that contains all file paths as one string) to a list of
-- file paths.
unlinesTree :: (String, String) -> [(String, String)]
unlinesTree (commit, filesString) = zip (repeat commit) files
    where files = filter (not . null) $ splitOn "\n" filesString

{-
  @Issue(
    "Make this function a bit more readable"
    type="improvement"
    priority="low"
    labels="readability"
  )
  @Issue(
    "Consider moving this to the Parser module"
    type="improvement"
    priority="low"
    labels="modularity"
  )
-}
-- Parse a list of trees (that contains a list of multiple file contents for
-- multiple commits) and return a list of Issues.
parseTrees :: [(String, String, String)] -> [Issue]
parseTrees input = concat [bulkSetProperty (bulkSetProperty (parseString content) "commit" commit) "file" filepath | (commit, filepath, content) <- input]

-- Functions for internal use.

createCommand :: String -> FilePath -> CreateProcess
createCommand command directory = (shell command){std_out = CreatePipe, std_err = CreatePipe, cwd = Just directory}
