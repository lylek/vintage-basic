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
