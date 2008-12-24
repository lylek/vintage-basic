#!/usr/local/bin/runhaskell

import Data.Array ((!))
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import System.Cmd (system)
import System.Directory (doesDirectoryExist,getCurrentDirectory,getDirectoryContents)
import System.FilePath ((</>))
import System.Environment (getArgs)
import Text.Regex.Base
import Text.Regex.Posix

testFilePat = makeRegex "^.*_test.hs$" :: Regex
testFuncPat = makeRegex "^ *(test_[A-Za-z0-9_']*) *=" :: Regex
testModuleNamePat = makeRegex "^ *module *([A-Za-z0-9_'.]*)" :: Regex

main = do
   args <- getArgs
   testModulePaths <- if null args then findModulePaths else return args
   putStrLn "Running tests in files:"
   sequence_ [putStrLn ("  " ++ testModulePath) | testModulePath <- testModulePaths]
   putStrLn ""
   putStrLn "Tests found:"
   modulesWithTests <-
       sequence [do moduleCode <- readFile testModulePath
                    let testModule = findModuleName moduleCode
                    putStrLn ("  " ++ testModule)
                    let tests = findTests moduleCode
                    sequence_ [putStrLn ("    " ++ test) | test <- tests]
                    return (testModule, findTests moduleCode)
                 | testModulePath <- testModulePaths]
   let testCode = genTestDriver modulesWithTests
   writeFile "test_driver.hs" testCode
   system ("runhaskell -itest -isrc test_driver.hs")

findFilesInSubdirs :: FilePath -> IO [String]
findFilesInSubdirs dir = do
    files <- getDirectoryContents dir
    let paths = [dir </> file | file <- filter (`notElem` [".", ".."]) files]
    paths' <- sequence [do { t <- doesDirectoryExist path; if t then findFilesInSubdirs path else return [] } | path <- paths]
    return $ concat (paths : paths')

findModulePaths :: IO [String]
findModulePaths = do
    files <- findFilesInSubdirs "test"
    return $ map head $ concat [match testFilePat file | file <- files]

matchTextToSubstring :: MatchText String -> String
matchTextToSubstring mt = fst (mt ! 1)

findModuleName :: String -> String
findModuleName str =
    case matchOnceText testModuleNamePat str of
        Nothing -> error "Unable to find module name in file"
        (Just (_, mt, _)) -> matchTextToSubstring mt

findTests :: String -> [String]
findTests str = [matchTextToSubstring mt | mt <- matchAllText testFuncPat str]

genTestDriver modulesWithTests =
    "import System.Exit\n"
    ++ "import Test.HUnit\n"
    ++ concat [genImport testModule | (testModule,_) <- modulesWithTests]
    ++ "\n"
    ++ "main = do\n"
    ++ "   (Counts cases tried errors failures) <- runTestTT $\n"
    ++ "      TestList [\n"
    ++ concat (intersperse ",\n"
                 [genTest testModule testFunc
                  | (testModule,testFuncs) <- modulesWithTests, testFunc <- testFuncs])
    ++ "\n"
    ++ "               ]\n"
    ++ "   exitWith $ if errors > 0 || failures > 0\n"
    ++ "                then ExitFailure (errors+failures)\n"
    ++ "                else ExitSuccess\n"

genImport testModule = "import qualified " ++ testModule ++ "\n"

genTest testModule testFunc =
    let qualifiedTest = testModule ++ "." ++ testFunc
       in "                TestLabel \"" ++ qualifiedTest ++ "\" " ++ qualifiedTest
