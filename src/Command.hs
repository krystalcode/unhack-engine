{-
  @Issue(
    "Standardise the way imports are ordered",
    type="task",
    priority="low",
    labels="coding-standards"
  )
-}
import System.IO
import System.Environment
import Unhack.ElasticSearch
import Unhack.Parser
import Unhack.Issue
import Unhack.Filter

{-
  @Issue(
    "Use cmdargs package for building a better command line tool",
    type="improvement",
    priority="normal"
  )
-}
main = do
       (command:args) <- getArgs
       let (Just action) = lookup command dispatch
       action args

-- Dispatch the appropriate function.
dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("analyse", analyse)
            , ("store", store)
            ]

-- Display issues on CLI.
{-
  @Issue(
    "Accept paths as well as argument instead of individual files only",
    type="feature",
    priority="high"
  )
-}
analyse :: [String] -> IO ()
analyse [file, filterValue] = do
                              inh <- openFile file ReadMode
                              inpStr <- hGetContents inh
                              let issues = parseString inpStr
                              let filteredIssues = filterIssues issues filterValue
                              let updatedIssues = bulkSetProperty filteredIssues "file" file
                              putStr (unlines . map displayIssue $ updatedIssues)
                              hClose inh

-- Store issues to ElasticSearch.
{-
  @Issue(
    "Write the API in a storage-agnostic way",
    type="feature",
    priority="low",
    labels="modularity"
  )
-}
store :: [String] -> IO ()
store [file] = do
               inh <- openFile file ReadMode
               outh <- openFile "issues" WriteMode
               inpStr <- hGetContents inh
               let issues = parseString inpStr
               let updatedIssues = bulkSetProperty issues "file" file
               responses <- sequence . map (withBH' . indexIssue) $ updatedIssues
               hPutStr outh (unlines . map show $ responses)
               hClose inh
               hClose outh
