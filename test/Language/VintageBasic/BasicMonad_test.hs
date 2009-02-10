module Language.VintageBasic.BasicMonad_test where

import Data.List(intercalate)
import Test.HUnit
import Language.VintageBasic.Asserts
import Language.VintageBasic.BasicMonad
import Control.Monad.CPST.DurableTraps
import IO.IOStream

expectOutput expected code = TestCase $ do
    input <- noInput
    output <- mkOutput
    runProgram input output (code >> done)
    assertOutputEq output expected

test_printString =
    expectOutput "first\nsecondthird" $ do
        printString "first\nsecond"
        printString "third"

test_output_column =
    expectOutput "hello\n0;5;0" $ do
        col1 <- getOutputColumn
        printString "hello"
        col2 <- getOutputColumn
        printString "\n"
        col3 <- getOutputColumn
        printString (intercalate ";" [show c | c <- [col1, col2, col3]])

test_getNextChar = TestCase $ do
    input <- mkInput "abc"
    output <- mkOutput
    runProgram input output $ do
        c1 <- getNextChar 
        c2 <- getNextChar 
        c3 <- getNextChar 
        printString [c3,c1,c2]
        done
    assertOutputEq output "cab"
