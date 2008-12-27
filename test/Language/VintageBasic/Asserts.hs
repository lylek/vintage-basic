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

noInput :: IO IOStream
noInput = mkInput ""

mkOutput :: IO IOStream
mkOutput = do
    output <- newIORef ""
    return $ IOStream output

assertOutputEq output expected = do
    got <- vGetContents output
    assertEqual "Output not as expected" expected got

withCol taggedToks = [(sourceColumn pos, tok) | (Tagged pos tok) <- taggedToks]

withLineAndCol taggedToks = [(sourceLine pos, sourceColumn pos, tok) | (Tagged pos tok) <- taggedToks]

parserWithEof parser = do
    result <- parser
    eof <?> "END OF INPUT"
    return result

assertParseResult normalizeResult normalizeError parser input expected = do
    case parse (parserWithEof parser) "" input of
        (Left err) -> assertFailure ("Parse error: " ++ show (normalizeError err))
        (Right rls) -> assertEqual "Parse result not as expected" expected (normalizeResult rls)

assertParseError normalizeResult normalizeError parser input expected = do
    case parse (parserWithEof parser) "" input of
        (Left err) -> assertBool
            ("Parser reported wrong error.\n\nGot:\n" ++ show (normalizeError err) ++ "\n\nExpected error to contain:\n" ++ expected)
            (isInfixOf expected (show (normalizeError err)))
        (Right rls) -> assertFailure ("Parser didn't report error, instead got result: " ++ show (normalizeResult rls))

assertIOError code expectedError = do
    catch
        (code >> assertFailure "Code didn't fail as expected")
        (\err -> assertBool
            ("Error message '" ++ (show err) ++ "' didn't contain expected error '" ++ expectedError)
            (isInfixOf expectedError (show err))
        )
