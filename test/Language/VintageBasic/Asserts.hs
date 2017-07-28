{-# LANGUAGE FlexibleContexts #-}

module Language.VintageBasic.Asserts where

import Data.IORef
import Data.List(isInfixOf)
import Test.HUnit
import Text.ParserCombinators.Parsec(eof,parse,sourceLine,sourceColumn,(<?>))
import Language.VintageBasic.LexCommon
import IO.IOStream

-- | Make an input stream for testing.
mkInput :: String -> IO IOStream
mkInput s = do
    input <- newIORef s
    return $ IOStream input

-- | Make an empty input stream for testing.
noInput :: IO IOStream
noInput = mkInput ""

-- | Make an empty output stream for testing.
mkOutput :: IO IOStream
mkOutput = do
    output <- newIORef ""
    return $ IOStream output

-- | Assert the output of a program, with empty input.
assertOutputEq output expected = do
    got <- vGetContents output
    assertEqual "Output not as expected" expected got

-- | Extracts line number and token from tagged tokens. Useful for asserting these when you don't care about column number.
withCol taggedToks = [(sourceColumn pos, tok) | (Tagged pos tok) <- taggedToks]

-- | Extracts line number, column number, and token from tagged tokens. Useful for assertion position and token value.
withLineAndCol taggedToks = [(sourceLine pos, sourceColumn pos, tok) | (Tagged pos tok) <- taggedToks]

-- | A parser that wraps a check for end of input around another parser.
-- Useful when checking isolated expressions that don't normally reach end of input.
parserWithEof parser = do
    result <- parser
    eof <?> "END OF INPUT"
    return result

-- | Assert a parse result, expecting success.
assertParseResult normalizeResult normalizeError parser input expected = do
    case parse (parserWithEof parser) "" input of
        (Left err) -> assertFailure ("Parse error: " ++ show (normalizeError err))
        (Right rls) -> assertEqual "Parse result not as expected" expected (normalizeResult rls)

-- | Assert an expected parse error.
assertParseError normalizeResult normalizeError parser input expected = do
    case parse (parserWithEof parser) "" input of
        (Left err) -> assertBool
            ("Parser reported wrong error.\n\nGot:\n" ++ show (normalizeError err) ++ "\n\nExpected error to contain:\n" ++ expected)
            (isInfixOf expected (show (normalizeError err)))
        (Right rls) -> assertFailure ("Parser didn't report error, instead got result: " ++ show (normalizeResult rls))
