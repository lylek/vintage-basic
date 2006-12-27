module BasicLineScanner_test where

import Data.List(isInfixOf)
import Test.HUnit
import Text.ParserCombinators.Parsec
import BasicLineScanner

test_LineScanner = TestCase $ do
  let text = unlines ["10SKDJF@#"," 5   ASJDKFdf "]
  case parse rawLinesP "" text of
    (Left err) -> assertFailure ("parse error: " ++ show err)
    (Right rls) -> assertEqual "" [RawLine 10 3 "SKDJF@#", RawLine 5 6 "ASJDKFdf "] rls

test_reports_error_if_line_doesn't_start_with_number = TestCase $ do
  let text = unlines ["10SKDJF@#","ASJD4KFdf "]
  case parse rawLinesP "" text of
    (Left err) -> assertBool ("Parser reported wrong error:" ++ show err)
                               (isInfixOf "expecting line number or end of file" (show err))
    (Right rls) -> assertFailure "Parser didn't report error"

test_reports_error_if_file_doesn't_end_in_newline = TestCase $ do
  let text = "10SKDJF@#"
  case parse rawLinesP "" text of
    (Left err) -> assertBool ("Parser reported wrong error:" ++ show err)
                               (isInfixOf "unexpected end of input" (show err))
    (Right rls) -> assertFailure "Parser didn't report error"
