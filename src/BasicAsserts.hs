module BasicAsserts where

import Data.List(isInfixOf)
import Test.HUnit
import Text.ParserCombinators.Parsec(parse,sourceLine,sourceColumn)
import BasicLexCommon

assertTaggedParseResult parser input expected =
    let normalize taggedToks = [(sourceLine pos, sourceColumn pos, tok) | (Tagged pos tok) <- taggedToks] in
        assertParseResult parser input normalize expected

assertColAndParseResult parser input expected =
    let normalize taggedToks = [(sourceColumn pos, tok) | (Tagged pos tok) <- taggedToks] in
        assertParseResult parser input normalize expected

assertParseResult parser inputText normalize expected = do
    case parse parser "" inputText of
        (Left err) -> assertFailure ("Parse error: " ++ show err)
        (Right rls) -> assertEqual "Parse result not as expected" expected (normalize rls)

assertParseError parser inputText expectedError = do
    case parse parser "" inputText of
        (Left err) -> assertBool ("Parser reported wrong error: " ++ show err) (isInfixOf expectedError (show err))
        (Right rls) -> assertFailure "Parser didn't report error"

assertIOError code expectedError = do
    catch
        (code >> assertFailure "Code didn't fail as expected")
        (\err -> assertBool
            ("Error message '" ++ (show err) ++ "' didn't contain expected error '" ++ expectedError)
            (isInfixOf expectedError (show err))
        )
