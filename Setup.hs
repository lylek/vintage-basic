#!/usr/local/bin/runhaskell

import Distribution.Simple
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import System.Cmd (system)
import Distribution.Simple.LocalBuildInfo
import System.Directory (setCurrentDirectory,removeFile)

main = defaultMainWithHooks (simpleUserHooks {runTests = runAllTests})

runAllTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runAllTests a b pd lb = do
    setCurrentDirectory "src"
    system "runhaskell run_tests.hs"
    removeFile "test_driver.hs"
    return ()
