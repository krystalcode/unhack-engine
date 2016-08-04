module Main where


-- Imports.

-- External dependencies.

import System.Environment (getArgs)

-- Internal dependencies.

import Unhack.Cmd.Indexes
import Unhack.Cmd.Modes
import Unhack.Cmd.PubSub


-- Program's main entry.

main = do

    -- Get the arguments passed on by the command line.
    args <- getArgs
    cmd  <- getCmd args

    -- Delegate execution to the appropriate function.
    case cmd of
        CmdIndexes{} -> runIndexes cmd
        CmdPubSub{}  -> runPubSub cmd


-- Issues.

{-
    @Issue(
        "Develop and support an improved annotation style"
        type="feature"
        priority="high"
        labels="1.0.0-beta1"
    )
    @Issue(
        "Move these issues to the unhack.yaml file"
        type="task"
        priority="low"
    )
    @Issue(
        "Add configuration options for defining colors per property"
        type="feature"
        priority="low"
        labels="ux"
    )
    @Issue(
        "Remove screen printing and add proper logging and error management
        throughout the application"
        type="feature"
        priority="normal"
        labels="1.0.0-beta1, log management"
    )
-}
