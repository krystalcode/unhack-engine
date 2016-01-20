{-# LANGUAGE OverloadedStrings #-}

module Unhack.Process
       ( lazyProcess
       , strictProcess ) where

import Control.Exception (evaluate)
import qualified Data.Text as T (pack, unpack, Text)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (hGetContents, hSetEncoding, utf8)
import System.Process (createProcess, CreateProcess(..), StdStream(CreatePipe), shell, waitForProcess)


-- Public API.

{-
    @Issue(
        "Is there any drawback in setting utf8 encoding on all processes?"
        type="bug"
        priority="low"
    )
-}

lazyProcess :: T.Text -> FilePath -> IO (T.Text)
lazyProcess command directory = do
    (_, Just hout, Just herr, procHandle) <- createProcess $ createCommand command directory
    hSetEncoding hout utf8
    hSetEncoding herr utf8
    exitCode <- waitForProcess procHandle
    stdOut   <- hGetContents hout
    stdErr   <- hGetContents herr
    if exitCode == ExitSuccess
        then return  $ T.pack stdOut
        else error $ stdErr ++ stdOut

strictProcess :: T.Text -> FilePath -> IO (T.Text)
strictProcess command directory = do
    (_, Just hout, Just herr, procHandle) <- createProcess $ createCommand command directory
    hSetEncoding hout utf8
    hSetEncoding herr utf8
    stdOut   <- hGetContents hout
    evaluate (length stdOut)
    stdErr   <- hGetContents herr
    evaluate (length stdErr)
    exitCode <- waitForProcess procHandle
    if exitCode == ExitSuccess
        then return $ T.pack stdOut
        else error $ stdErr ++ stdOut


-- Functions for internal use.

createCommand :: T.Text -> FilePath -> CreateProcess
createCommand command directory = (shell $ T.unpack command){std_out = CreatePipe, std_err = CreatePipe, cwd = Just directory}
