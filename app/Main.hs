module Main where


-- Imports.

import System.Environment (getArgs)
import Unhack.Cmd.Indexes
import Unhack.Cmd.Modes
import Unhack.Cmd.PubSub
import Unhack.Cmd.Test


-- Program's main entry.

main = do

    -- Get the arguments passed on by the command line.
    args <- getArgs
    cmd  <- getCmd args

    -- Delegate execution to the appropriate function.
    case cmd of
        CmdIndexes{} -> runIndexes cmd
        CmdPubSub{}  -> runPubSub cmd
        CmdTest{}    -> runTest cmd


-- Issues.

{-
    @Issue(
        "Rename the program to unhack-engine"
        type="task"
        priority="low"
    )
    @Issue(
        "Develop and support an improved annotation style"
        type="feature"
        priority="high"
        labels="release"
    )
    @Issue(
        "Move these issues to the unhack.yaml file"
        type="task"
        priority="low"
    )
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
    @Issue(
        "Remove all screen printing in production environment"
        type="improvement"
        priority="low"
        labels="performance"
    )
-}
