module BasicExecuter_test where

import Data.List(isInfixOf)
import Test.HUnit
import Text.ParserCombinators.Parsec.Pos(newPos)
import BasicAsserts
import BasicExecuter

--test_tokenization_errors_reported_at_correct_location = TestCase $ do
--    let source = (Tagged (newPos "bobble.bas" 10 4) "!")
--    let expectedError = "(line 10, column 4)"
--    case tokenizeRawLine source of
--        (Left err) -> assertBool ("Parser reported wrong error. Got:\n" ++ show err ++ "\nExpected error to contain:\n" ++ expectedError) (isInfixOf expectedError (show err))
--        (Right rls) -> assertFailure "Parser didn't report error"
