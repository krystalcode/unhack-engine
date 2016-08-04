{-# LANGUAGE OverloadedStrings #-}

module Unhack.Process
       ( lazyProcess
       , strictProcess ) where


-- Imports.

import Control.Exception (handle, evaluate, SomeException(..))
import System.Exit       (ExitCode(ExitSuccess, ExitFailure))
import System.IO         (hGetContents, hSetEncoding, utf8)
import System.Process    (createProcess, CreateProcess(..), StdStream(CreatePipe), shell, waitForProcess)

import qualified Data.Text as T (pack, unpack, Text)


-- Public API.

{-
    @Issue(
        "Is there any drawback in setting utf8 encoding on all processes?"
        type="bug"
        priority="low"
    )
    @Issue(
        "Implement error handling for the lazyProcess function as well"
        type="bug"
        priority="normal"
        label="error management"
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

strictProcess :: T.Text -> FilePath -> IO (Either ExitCode T.Text)
strictProcess command directory = handle (\e -> handleException e) $ do
    (_, Just hout, Just herr, procHandle) <- createProcess $ createCommand command directory
    hSetEncoding hout utf8
    hSetEncoding herr utf8
    stdOut   <- hGetContents hout
    evaluate (length stdOut)
    stdErr   <- hGetContents herr
    evaluate (length stdErr)
    exitCode <- waitForProcess procHandle

    {-
      @Issue(
        "Capture and return the exception message as well"
        type="improvement"
        priority="low"
        labels="error management"
      )
    -}
    if exitCode == ExitSuccess
        then return $ Right (T.pack stdOut)
        else return $ Left exitCode --stdErr ++ stdOut ++ (show exitCode)

    -- If an exception is thrown (instead of the process ending with failure),
    -- then we return an ExitFailure code of 0. This way the function that
    -- requested the process will know that an unknown IO Exception has
    -- occurred.
    {-
      @Issue(
        "Differentiate between IOException and other exceptions"
        type="improvement"
        priority="low"
        labels="error management"
      )
    -}
    where handleException (SomeException e) = return $ Left (ExitFailure 0)


-- Functions for internal use.

createCommand :: T.Text -> FilePath -> CreateProcess
createCommand command directory = (shell $ T.unpack command){std_out = CreatePipe, std_err = CreatePipe, cwd = Just directory}
