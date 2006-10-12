#!/usr/local/bin/runhaskell

import Data.List (intersperse)
import Data.Maybe (catMaybes)
import System.Cmd (system)
import System.Directory (getCurrentDirectory,getDirectoryContents)
import System.Environment (getArgs)
import Text.Regex (mkRegexWithOpts,matchRegex,matchRegexAll)

testFilePat = mkRegexWithOpts "^(.*_test).hs$" False False
testFuncPat = mkRegexWithOpts "^ *(test_[A-Z0-9_']*) *=.*$" True False

main = do
   args <- getArgs
   testModules <- if null args then findModules else return args
   putStrLn "Testing modules:"
   sequence_ [putStrLn ("  " ++ testModule) | testModule <- testModules]
   modulesWithTests <-
       sequence [do moduleCode <- readFile (testModule ++ ".hs")
                    putStrLn ("  " ++ testModule)
	            let tests = findTests moduleCode
	            sequence_ [putStrLn ("    " ++ test) | test <- tests]
                    return (testModule, findTests moduleCode)
                 | testModule <- testModules]
   let testCode = genTestDriver modulesWithTests
   writeFile "test_driver.hs" testCode
   system ("runhaskell test_driver.hs")
  
findModules :: IO [String]
findModules = do
   curDir <- getCurrentDirectory
   files <- getDirectoryContents curDir
   return $ concat $ catMaybes [matchRegex testFilePat file | file <- files]

findTests :: String -> [String]
findTests str =
    case matchRegexAll testFuncPat str of
       Nothing -> []
       (Just (_, _, rest, [testFunc])) -> testFunc : findTests rest
       _ -> error "Impossible error: Match did not have one subexpression"

genTestDriver modulesWithTests =
    "import Test.HUnit\n"
    ++ concat [genImport testModule | (testModule,_) <- modulesWithTests]
    ++ "\n"
    ++ "main = runTestTT $\n"
    ++ "   TestList [\n"
    ++ concat (intersperse ",\n"
                 [genTest testModule testFunc
                  | (testModule,testFuncs) <- modulesWithTests, testFunc <- testFuncs])
    ++ "\n"
    ++ "      ]\n"

genImport testModule = "import qualified " ++ testModule ++ "\n"

genTest testModule testFunc =
    let qualifiedTest = testModule ++ "." ++ testFunc
       in "      TestLabel \"" ++ qualifiedTest ++ "\" " ++ qualifiedTest

{-
main = runTestTT $
       TestList [TestLabel "test_LineScanner" test_LineScanner,
                 TestLabel "test_reports_error_if_line_doesn't_start_with_number"
                   test_reports_error_if_line_doesn't_start_with_number,
                 TestLabel "test_reports_error_if_file_doesn't_end_in_newline"
                   test_reports_error_if_file_doesn't_end_in_newline
                ]
-}
