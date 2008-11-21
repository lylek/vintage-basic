module BasicParser_test where

import Test.HUnit
import Text.ParserCombinators.Parsec(parse)
import Text.ParserCombinators.Parsec.Pos
import BasicLexCommon(Tagged(..))
import BasicParser
import BasicResult
import BasicSyntax
import BasicTokenizer(taggedTokensP)

taggedValToColAndVal (Tagged pos val) = (sourceColumn pos, val)

parserTest source expectedColAndStatements = TestCase $ do
  let (Right taggedToks) = parse taggedTokensP "" source
  let result = parse statementListP "" taggedToks
  case result of
      (Left err) -> assertFailure ("parse error: " ++ show (SyntaxError err))
      (Right taggedStatements) ->
          assertEqual "" expectedColAndStatements (map taggedValToColAndVal taggedStatements)

test_parse = parserTest
    "GOTO12"
    [(1,GotoS 12)]

test_parses_multiple_statements = parserTest
    ":?::?:"
    [(2,PrintS [] True), (5,PrintS [] True)]
 
test_parse_bare_print = parserTest
    "PRINT"
    [(1,PrintS [] True)]
 
test_parse_print_string = parserTest
    "PRINT\"hello\""
    [(1,PrintS [LitX (StringLit "hello")] True)]
 
test_parse_print_string_wo_newline = parserTest
    "PRINT\"hello\";"
    [(1,PrintS [LitX (StringLit "hello")] False)]
 
test_parse_print_with_semicolon_separated_parts = parserTest
    "PRINT\"hello\";\"there\""
    [(1,PrintS [LitX (StringLit "hello"), LitX (StringLit "there")] True)]
 
test_parse_print_with_juxtaposed_parts = parserTest
    "PRINT\"hello\"\"there\""
    [(1,PrintS [LitX (StringLit "hello"), LitX (StringLit "there")] True)]

test_parse_print_with_comma_separated_parts = parserTest
    "PRINT\"hello\",\"there\""
    [(1,PrintS [LitX (StringLit "hello"), NextZoneX, LitX (StringLit "there")] True)]
 
test_parse_let = parserTest
    "LETA=1"
    [(1,LetS (ScalarVar (VarName FloatType "A")) (LitX (FloatLit 1.0)))]
 
test_parse_let_wo_keyword = parserTest
    "A=1"
    [(1,LetS (ScalarVar (VarName FloatType "A")) (LitX (FloatLit 1.0)))]

test_parse_multiple_dims = parserTest
    "DIMA$(5),G(14,20)"
    [(1,DimS [(VarName StringType "A", [(LitX (FloatLit 5))]), (VarName FloatType "G", [(LitX (FloatLit 14)), (LitX (FloatLit 20))])])]

test_parse_goto = parserTest
    "GOTO20"
    [(1,GotoS 20)]

test_parse_gosub = parserTest
    "GOSUB20"
    [(1,GosubS 20)]

test_parse_on_goto = parserTest
    "ON3GOTO10,20,40"
    [(1,OnGotoS (LitX (FloatLit 3)) [10,20,40])]

test_parse_on_gosub = parserTest
    "ON3GOSUB10,20,40"
    [(1,OnGosubS (LitX (FloatLit 3)) [10,20,40])]

test_parse_data = parserTest
    "DATA4,5,\"THIS,WORKS\""
    [(1,DataS "4,5,\"THIS,WORKS\"")]

test_parse_read = parserTest
    "READA$(5),B"
    [(1,ReadS [ArrVar (VarName StringType "A") [(LitX (FloatLit 5))], ScalarVar (VarName FloatType "B")])]

test_parse_restore = parserTest
    "RESTORE"
    [(1,RestoreS Nothing)]

test_parse_restore_with_line_number = parserTest
    "RESTORE20"
    [(1,RestoreS (Just 20))]

test_parse_def_fn = parserTest
    "DEFFNAN1$(B,CD$)=4+B"
    [(1,DefFnS (VarName StringType "AN1") [(VarName FloatType "B"), (VarName StringType "CD")] (BinX AddOp (LitX (FloatLit 4)) (VarX (ScalarVar (VarName FloatType "B")))))]

test_parse_fn = parserTest
    "?FNAN2$(1,\"X\")"
    [(1,PrintS [FnX (VarName StringType "AN2") [(LitX (FloatLit 1)), (LitX (StringLit "X"))]] True)]
