module BasicParser_test where

--import Data.List(isInfixOf)
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import BasicLexCommon(Tagged(..))
import BasicParser
import BasicSyntax
import BasicTokenizer

taggedValToColAndVal (Tagged pos val) = (sourceColumn pos, val)

makeValidParseTest colAndToks expectedColAndStatements = TestCase $ do
  let taggedToks = [Tagged (newPos "" 1 col) tok | (col,tok) <- colAndToks]
  let result = parse statementListP "" taggedToks
  case result of
           (Left err) -> assertFailure ("parse error: " ++ show err)
           (Right taggedStatements) ->
               assertEqual "" expectedColAndStatements (map taggedValToColAndVal taggedStatements)

test_parse =
    makeValidParseTest
    [(1,GoTok), (3,ToTok), (5,CharTok '1'), (6,CharTok '2')]
    [(1,GotoS 12)]
