module BasicAsserts where

import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.List(isInfixOf)
import Test.HUnit
import Text.ParserCombinators.Parsec(parse,sourceLine,sourceColumn)
import BasicLexCommon
import IOStream

mkInput :: String -> IO IOStream
mkInput s = do
    input <- newIORef (BS.pack s)
    return $ IOStream input

noInput :: IO IOStream
noInput = mkInput ""

mkOutput :: IO IOStream
mkOutput = do
    output <- newIORef (BS.pack "")
    return $ IOStream output

assertTaggedParseResult normalizeError parser input expected =
    let normalizeResult taggedToks = [(sourceLine pos, sourceColumn pos, tok) | (Tagged pos tok) <- taggedToks] in
        assertParseResult normalizeError parser input normalizeResult expected

assertColAndParseResult normalizeError parser input expected =
    let normalizeResult taggedToks = [(sourceColumn pos, tok) | (Tagged pos tok) <- taggedToks] in
        assertParseResult normalizeError parser input normalizeResult expected

assertParseResult normalizeError parser inputText normalizeResult expected = do
    case parse parser "" inputText of
        (Left err) -> assertFailure ("Parse error: " ++ show (normalizeError err))
        (Right rls) -> assertEqual "Parse result not as expected" expected (normalizeResult rls)

assertParseError normalizeError parser inputText expectedError = do
    case parse parser "" inputText of
        (Left err) -> assertBool
            ("Parser reported wrong error: " ++ show (normalizeError err))
            (isInfixOf expectedError (show (normalizeError err)))
        (Right rls) -> assertFailure "Parser didn't report error"

assertIOError code expectedError = do
    catch
        (code >> assertFailure "Code didn't fail as expected")
        (\err -> assertBool
            ("Error message '" ++ (show err) ++ "' didn't contain expected error '" ++ expectedError)
            (isInfixOf expectedError (show err))
        )
