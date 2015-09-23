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
import Unhack.Commit

import System.IO
import System.Process
import System.Exit
import Data.List
import Data.List.Split

-- Public API.

listCommitsIO :: FilePath -> IO (String)
listCommitsIO directory = do
    (_, Just hout, Just herr, procHandle) <- createProcess $ createCommand "git log --pretty=\"format:%H_%ai\"" directory
    exitCode <- waitForProcess procHandle
    stdOut   <- hGetContents hout
    stdErr   <- hGetContents herr
    if exitCode == ExitSuccess
        then return stdOut
        else error $ stdErr ++ stdOut

listCommits :: String -> [Commit]
listCommits [] = []
listCommits input = map stringToCommit lines
    where lines = splitOn "\n" input

-- Get the contents of a file on a specific commit.
showFileIO :: String -> (Commit, String) -> IO (Commit, String, String)
showFileIO directory (commit, filepath) = do
  (_, Just hout, Just herr, procHandle) <- createProcess $ createCommand command directory
  exitCode <- waitForProcess procHandle
  stdOut   <- hGetContents hout
  stdErr   <- hGetContents herr
  if exitCode == ExitSuccess
     then return (commit, filepath, stdOut)
     -- Continue in the case of an error, such as when the file does not exist in the commit.
     else return (commit, filepath, "")
  where command = "git show " ++ (hash commit) ++ ":" ++ filepath

-- Get the full git tree on a specific commit.
lsTreeIO :: FilePath -> Commit -> IO (Commit, String)
lsTreeIO directory commit = do
    (_, Just hout, Just herr, procHandle) <- createProcess $ createCommand ("git ls-tree --full-tree --name-only -r " ++ (hash commit)) directory
    exitCode <- waitForProcess procHandle
    stdOut   <- hGetContents hout
    stdErr   <- hGetContents herr
    if exitCode == ExitSuccess
        then return (commit, stdOut)
        else error $ stdErr ++ stdOut

-- Convert a git tree (that contains all file paths as one string) to a list of
-- file paths.
unlinesTree :: (Commit, String) -> [(Commit, String)]
unlinesTree (commit, filesString) = zipWithCommit commit files
    where files = filter (not . null) $ splitOn "\n" filesString
          zipWithCommit commitRecord filesList = [(commitRecord, fileString) | fileString <- filesList]

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
parseTrees :: [(Commit, String, String)] -> [Issue]
parseTrees input = concat [bulkSetProperty (bulkSetCommit (parseString content) commit) "file" filepath | (commit, filepath, content) <- input]

-- Functions for internal use.

createCommand :: String -> FilePath -> CreateProcess
createCommand command directory = (shell command){std_out = CreatePipe, std_err = CreatePipe, cwd = Just directory}
