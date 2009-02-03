module Language.VintageBasic.Parser_test where

import Test.HUnit
import Text.ParserCombinators.Parsec(parse)
import Text.ParserCombinators.Parsec.Pos
import Language.VintageBasic.Asserts
import Language.VintageBasic.Builtins
import Language.VintageBasic.LexCommon(Tagged(..))
import Language.VintageBasic.Parser
import Language.VintageBasic.Result
import Language.VintageBasic.Syntax
import Language.VintageBasic.Tokenizer(taggedTokensP)

tokenize source = let (Right taggedToks) = parse taggedTokensP "" source in taggedToks

goodStatements source expectedColAndStatements = TestCase $
  assertParseResult withCol SyntaxError statementListP (tokenize source) expectedColAndStatements

badStatements parser source expectedError = TestCase $
  assertParseError withCol SyntaxError statementListP (tokenize source) expectedError

goodExpr source expectedExpr = TestCase $
  assertParseResult id SyntaxError exprP (tokenize source) expectedExpr

badExpr source expectedError = TestCase $
  assertParseError id SyntaxError exprP (tokenize source) expectedError

test_parses_multiple_statements = goodStatements
    ":?::?: "
    [(2,PrintS [] True), (5,PrintS [] True)]
 
test_parse_bare_print = goodStatements
    "PRINT"
    [(1,PrintS [] True)]
 
test_parse_print_string = goodStatements
    "PRINT\"hello\""
    [(1,PrintS [LitX (StringLit "hello")] True)]
 
test_parse_print_string_wo_newline = goodStatements
    "PRINT\"hello\";"
    [(1,PrintS [LitX (StringLit "hello")] False)]
 
test_parse_print_with_semicolon_separated_parts = goodStatements
    "PRINT\"hello\";\"there\""
    [(1,PrintS [LitX (StringLit "hello"), LitX (StringLit "there")] True)]
 
test_parse_print_with_juxtaposed_parts = goodStatements
    "PRINT\"hello\"\"there\""
    [(1,PrintS [LitX (StringLit "hello"), LitX (StringLit "there")] True)]

test_parse_print_with_comma_separated_parts = goodStatements
    "PRINT\"hello\",\"there\""
    [(1,PrintS [LitX (StringLit "hello"), NextZoneX, LitX (StringLit "there")] True)]
 
test_parse_print_with_a_comma_at_the_start = goodStatements
    "PRINT,\"hello\""
    [(1,PrintS [NextZoneX, LitX (StringLit "hello")] True)]

test_parse_let = goodStatements
    "LETA=1"
    [(1,LetS (ScalarVar (VarName FloatType "A")) (LitX (FloatLit 1.0)))]
 
test_parse_let_wo_keyword = goodStatements
    "A=1"
    [(1,LetS (ScalarVar (VarName FloatType "A")) (LitX (FloatLit 1.0)))]

test_parse_multiple_dims = goodStatements
    "DIMA$(5),G(14,20)"
    [(1,DimS [(VarName StringType "A", [(LitX (FloatLit 5))]), (VarName FloatType "G", [(LitX (FloatLit 14)), (LitX (FloatLit 20))])])]

test_parse_goto = goodStatements
    "GOTO20"
    [(1,GotoS 20)]

test_ignores_space_after_target = goodStatements
    "GOTO20 "
    [(1,GotoS 20)]
 
test_parse_gosub = goodStatements
    "GOSUB20"
    [(1,GosubS 20)]

test_parse_on_goto = goodStatements
    "ON3GOTO10,20,40"
    [(1,OnGotoS (LitX (FloatLit 3)) [10,20,40])]

test_parse_on_gosub = goodStatements
    "ON3GOSUB10,20,40"
    [(1,OnGosubS (LitX (FloatLit 3)) [10,20,40])]

test_parse_data = goodStatements
    "DATA4,5,\"THIS,WORKS\""
    [(1,DataS "4,5,\"THIS,WORKS\"")]

test_parse_read = goodStatements
    "READA$(5),B"
    [(1,ReadS [ArrVar (VarName StringType "A") [(LitX (FloatLit 5))], ScalarVar (VarName FloatType "B")])]

test_parse_restore = goodStatements
    "RESTORE"
    [(1,RestoreS Nothing)]

test_parse_restore_with_line_number = goodStatements
    "RESTORE20"
    [(1,RestoreS (Just 20))]

test_parse_def_fn = goodStatements
    "DEFFNAN1$(B,CD$)=4+B"
    [(1,DefFnS (VarName StringType "AN1") [(VarName FloatType "B"), (VarName StringType "CD")] (BinX AddOp (LitX (FloatLit 4)) (VarX (ScalarVar (VarName FloatType "B")))))]

test_parse_fn = goodStatements
    "?FNAN2$(1,\"X\")"
    [(1,PrintS [FnX (VarName StringType "AN2") [(LitX (FloatLit 1)), (LitX (StringLit "X"))]] True)]

test_parse_end = goodStatements
    "END"
    [(1,EndS)]

test_parse_stop = goodStatements
    "STOP"
    [(1,StopS)]

fl v = LitX (FloatLit v)
sl s = LitX (StringLit s)
fv s = VarX (ScalarVar (VarName FloatType s))
iv s = VarX (ScalarVar (VarName IntType s))
sv s = VarX (ScalarVar (VarName StringType s))

test_primitive_expressions = TestList [
    "round number"  ~: goodExpr "234" (fl 234),
    "float"         ~: goodExpr "2.3" (fl 2.3),
    "string"        ~: goodExpr "\"Work\"" (sl "Work"),
    "builtin"       ~: goodExpr "SIN(1)" (BuiltinX SinBI [(fl 1)]),
    "builtin2"      ~: goodExpr "LEFT$(A$,1)" (BuiltinX LeftBI [sv "A", fl 1]),
    "fn"            ~: goodExpr "FNAB(1,2,3)" (FnX (VarName FloatType "AB") [fl 1, fl 2, fl 3]),
    "float var"     ~: goodExpr "BR" (fv "BR"),
    "int var"       ~: goodExpr "BR%" (iv "BR"),
    "string var"    ~: goodExpr "BR$" (sv "BR"),
    "array var"     ~: goodExpr "AB(20, 5)" (VarX (ArrVar (VarName FloatType "AB") [fl 20, fl 5]))
  ]

test_operators = TestList [
    "unary +"       ~: goodExpr "+A"   (fv "A"),
    "unary -"       ~: goodExpr "-A"   (MinusX (fv "A")),
    "num add"       ~: goodExpr "1+2"  (BinX AddOp (fl 1) (fl 2)),
    "num sub"       ~: goodExpr "1-2"  (BinX SubOp (fl 1) (fl 2)),
    "num mul"       ~: goodExpr "1*2"  (BinX MulOp (fl 1) (fl 2)),
    "num div"       ~: goodExpr "1/2"  (BinX DivOp (fl 1) (fl 2)),
    "num pow"       ~: goodExpr "1^2"  (BinX PowOp (fl 1) (fl 2)),
    "num eq"        ~: goodExpr "1=2"  (BinX EqOp  (fl 1) (fl 2)),
    "num ne"        ~: goodExpr "1<>2" (BinX NEOp  (fl 1) (fl 2)),
    "num lt"        ~: goodExpr "1<2"  (BinX LTOp  (fl 1) (fl 2)),
    "num gt"        ~: goodExpr "1>2"  (BinX GTOp  (fl 1) (fl 2)),
    "string concat" ~: goodExpr "\"A\"+\"B\"" (BinX AddOp (sl "A") (sl "B")),
    "string lt"     ~: goodExpr "\"A\"<\"B\"" (BinX LTOp  (sl "A") (sl "B")),
    "string gt"     ~: goodExpr "\"A\">\"B\"" (BinX GTOp  (sl "A") (sl "B")),
    "string eq"     ~: goodExpr "\"A\"=\"B\"" (BinX EqOp  (sl "A") (sl "B")),
    "and"           ~: goodExpr "1=2AND3=4" (BinX AndOp (BinX EqOp (fl 1) (fl 2)) (BinX EqOp (fl 3) (fl 4))),
    "or"            ~: goodExpr "1=2OR3=4"  (BinX OrOp (BinX EqOp (fl 1) (fl 2)) (BinX EqOp (fl 3) (fl 4))),
    "not"           ~: goodExpr "NOT1" (NotX (fl 1))
  ]

test_associativity = TestList [
    "or assoc"      ~: goodExpr "1OR2OR3" (BinX OrOp (BinX OrOp (fl 1) (fl 2)) (fl 3)),
    "and assoc"     ~: goodExpr "1AND2AND3" (BinX AndOp (BinX AndOp (fl 1) (fl 2)) (fl 3)),
    "eq assoc"      ~: goodExpr "1=2=3" (BinX EqOp (BinX EqOp (fl 1) (fl 2)) (fl 3)),
    "ne assoc"      ~: goodExpr "1<>2<>3" (BinX NEOp (BinX NEOp (fl 1) (fl 2)) (fl 3)),
    "lt assoc"      ~: goodExpr "1<2<3" (BinX LTOp (BinX LTOp (fl 1) (fl 2)) (fl 3)),
    "le assoc"      ~: goodExpr "1<=2<=3" (BinX LEOp (BinX LEOp (fl 1) (fl 2)) (fl 3)),
    "gt assoc"      ~: goodExpr "1>2>3" (BinX GTOp (BinX GTOp (fl 1) (fl 2)) (fl 3)),
    "ge assoc"      ~: goodExpr "1>=2>=3" (BinX GEOp (BinX GEOp (fl 1) (fl 2)) (fl 3)),
    "add assoc"     ~: goodExpr "1+2+3" (BinX AddOp (BinX AddOp (fl 1) (fl 2)) (fl 3)),
    "sub assoc"     ~: goodExpr "1-2-3" (BinX SubOp (BinX SubOp (fl 1) (fl 2)) (fl 3)),
    "mul assoc"     ~: goodExpr "1*2*3" (BinX MulOp (BinX MulOp (fl 1) (fl 2)) (fl 3)),
    "div assoc"     ~: goodExpr "1/2/3" (BinX DivOp (BinX DivOp (fl 1) (fl 2)) (fl 3)),
    "pow assoc"     ~: goodExpr "1^2^3" (BinX PowOp (fl 1) (BinX PowOp (fl 2) (fl 3)))
  ]

test_precedence = TestList [
    "and beats or"      ~: goodExpr "1OR2AND3" (BinX OrOp (fl 1) (BinX AndOp (fl 2) (fl 3))),
    "not beats and"     ~: goodExpr "NOT1AND2" (BinX AndOp (NotX (fl 1)) (fl 2)),
    "eq beats not"      ~: goodExpr "NOT1=2"   (NotX (BinX EqOp (fl 1) (fl 2))),
    "eq class"          ~: goodExpr "1=2<>3<4<=5>6>=7" (BinX GEOp (BinX GTOp (BinX LEOp (BinX LTOp (BinX NEOp (BinX EqOp (fl 1) (fl 2)) (fl 3)) (fl 4)) (fl 5)) (fl 6)) (fl 7)),
    "eq class rev"      ~: goodExpr "1>=2>3<=4<5<>6=7" (BinX EqOp (BinX NEOp (BinX LTOp (BinX LEOp (BinX GTOp (BinX GEOp (fl 1) (fl 2)) (fl 3)) (fl 4)) (fl 5)) (fl 6)) (fl 7)),
    "add beats eq"      ~: goodExpr "1=2+3" (BinX EqOp (fl 1) (BinX AddOp (fl 2) (fl 3))),
    "add class"         ~: goodExpr "1+2-3" (BinX SubOp (BinX AddOp (fl 1) (fl 2)) (fl 3)),
    "add class rev"     ~: goodExpr "1-2+3" (BinX AddOp (BinX SubOp (fl 1) (fl 2)) (fl 3)),
    "mul beats add"     ~: goodExpr "1+2*3" (BinX AddOp (fl 1) (BinX MulOp (fl 2) (fl 3))),
    "mul class"         ~: goodExpr "1*2/3" (BinX DivOp (BinX MulOp (fl 1) (fl 2)) (fl 3)),
    "mul class rev"     ~: goodExpr "1/2*3" (BinX MulOp (BinX DivOp (fl 1) (fl 2)) (fl 3)),
    "pow beats mul"     ~: goodExpr "1*2^3" (BinX MulOp (fl 1) (BinX PowOp (fl 2) (fl 3))),
    "unary - beats pow" ~: goodExpr "-A^B" (BinX PowOp (MinusX (fv "A")) (fv "B"))
  ]

test_parentheses = TestList [
    goodExpr "(1+2)*3" (BinX MulOp (ParenX (BinX AddOp (fl 1) (fl 2))) (fl 3)),
    goodExpr "-(1+2)*3" (BinX MulOp (MinusX (ParenX (BinX AddOp (fl 1) (fl 2)))) (fl 3)),
    goodExpr "(1+(2-3))*4" (BinX MulOp (ParenX (BinX AddOp (fl 1) (ParenX (BinX SubOp (fl 2) (fl 3))))) (fl 4))
  ]

test_embedded_spaces_are_ignored = TestList [
    goodExpr "1 * 2  +3-  4" (BinX SubOp (BinX AddOp (BinX MulOp (fl 1) (fl 2)) (fl 3)) (fl 4)),
    goodExpr "CHR$ ( A ) " (BuiltinX ChrBI [fv "A"])
  ]

test_bad_expressions = TestList [
    "starting with operator" ~: badExpr "/4" "UNEXPECTED /",
    "ending with operator"   ~: badExpr "4/" "UNEXPECTED END OF LINE",
    "double operator"        ~: badExpr "4/*4" "UNEXPECTED *"
  ]
