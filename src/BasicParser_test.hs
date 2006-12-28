module BasicParser_test where

--import Data.List(isInfixOf)
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import BasicParser
import BasicSyntax
import BasicTokenizer

test_parse = TestCase $ do
  let colAndToks = [(1,GoTok), (3,ToTok), (5,CharTok '1'), (7, CharTok '2')]
  let posAndToks = [(newPos "" 1 col, tok) | (col,tok) <- colAndToks]
  let expected = [GotoS 12]
  let result = parse statementListP "" posAndToks
  case result of
           (Left err) -> assertFailure ("parse error: " ++ show err)
           (Right actual) -> assertEqual "" expected actual
