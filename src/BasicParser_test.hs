module BasicParser_test where

--import Data.List(isInfixOf)
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import BasicParser
import BasicSyntax
import BasicTokenizer

makeValidParseTest colAndToks statements = TestCase $ do
  let posAndToks = [(newPos "" 1 col, tok) | (col,tok) <- colAndToks]
  let result = parse lineP "" posAndToks
  case result of
           (Left err) -> assertFailure ("parse error: " ++ show err)
           (Right actual) -> assertEqual "" statements actual

test_parse =
    makeValidParseTest
    [(1,GoTok), (3,ToTok), (5,CharTok '1'), (6,CharTok '2')]
    [GotoS 12]
