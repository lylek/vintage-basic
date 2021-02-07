module Language.VintageBasic.Interpreter_test where

import Test.HUnit
import Language.VintageBasic.Asserts
import Language.VintageBasic.Executer
import Language.VintageBasic.BasicMonad
import Control.Monad.CPST.DurableTraps

testProgramOutput source expected = TestCase $ do
    i <- noInput
    o <- mkOutput
    runProgram i o (execute "" source >> done)
    assertOutputEq o expected

testProgramOutputWithInput source input expected = TestCase $ do
    i <- mkInput input
    o <- mkOutput
    runProgram i o (execute "" source >> done)
    assertOutputEq o expected

testExpression source expected = testProgramOutput ("1?" ++ source ++ "\n") (expected ++ "\n")

testExpressions ts = TestList $ map (uncurry testExpression) ts

test_does_nothing = testProgramOutput "" ""

test_executes_lines_in_sequence = testProgramOutput "10 ?\"A\"\n20 ?\"B\"\n" "A\nB\n"

test_var_defaults = testProgramOutput "10 ?A:?A%:?A$\n" " 0 \n 0 \n\n"

test_float_assignment = testProgramOutput "10 A=5:PRINTA\n" " 5 \n"

test_int_assignment_pos = testProgramOutput "10 A%=5.9:?A%\n" " 5 \n"

test_int_assignment_neg = testProgramOutput "10 A%=-5.1:?A%\n" "-6 \n"

test_string_assignment = testProgramOutput "10 A$=\"WOW\":?A$\n" "WOW\n"

test_variables_of_different_types_with_the_same_name_are_different = testProgramOutput
    "10 A=4.5:A%=3:A$=\"HI\"\n20 ?A:?A%:?A$\n"
    " 4.5 \n 3 \nHI\n"

test_var_names_are_only_significant_to_two_characters = testProgramOutput
    "10 SOMETHING=4:SAMETHING=2:?SOWHAT:?SALLY\n"
    " 4 \n 2 \n"

test_first_digit_of_var_name_is_significant_too = testProgramOutput
    "10 SOMETHING12=4:SOMETHING22=2:?SOMETHING13:?SOMETHING24\n"
    " 4 \n 2 \n"

test_dim = testProgramOutput "10 DIMA(20):A(0)=5:A(20)=6:PRINTA(20);A(0)\n" " 6  5 \n"

test_dim_default_size_is_at_least_10 = testProgramOutput "10 A(10)=6:PRINTA(10)\n" " 6 \n"

test_dim_default_size_is_at_most_10 = testProgramOutput
    "10 A(11)=6:PRINTA(11)\n" "!OUT OF ARRAY BOUNDS IN LINE 10\n"

test_dim_with_negative_dimension = testProgramOutput
    "10 DIMA(-1)\n"
    "!NEGATIVE ARRAY DIM IN LINE 10\n"

test_multiple_dimensions = testProgramOutput "10 DIMA(1,3):A(1,3)=12:PRINTA(1,3)\n" " 12 \n"

test_array_dim_rounding = testProgramOutput
    "10 DIMA(11.9):A(11.9)=5:?A(11.1):A(12)=6:?A(6)\n"
    " 5 \n!OUT OF ARRAY BOUNDS IN LINE 10\n"

test_for_loop_with_array = testProgramOutput "10 FORI=1TO10:A(I)=10-I:NEXT:PRINTA(3)\n" " 7 \n"

test_int_array = testProgramOutput "10 A%(5)=2.8:?A%(1);A%(5)\n" " 0  2 \n"

test_string_array = testProgramOutput "10 A$(5)=\"HI\":?A$(1);A$(5)\n" "HI\n"

test_arithmetic_expression = testProgramOutput "10 A=5:?A^2+3*A-1/2\n" " 39.5 \n"

test_parentheses = testProgramOutput "10 A=5:?A^(2+3)*(A-1)/2\n" " 6250 \n"

test_relational_operators_on_numbers = TestList $ [
    "eq" ~: testProgramOutput "10 ?1= 1:?1= 2:?2= 1\n" "-1 \n 0 \n 0 \n",
    "ne" ~: testProgramOutput "10 ?1<>1:?1<>2:?2<>1\n" " 0 \n-1 \n-1 \n",
    "lt" ~: testProgramOutput "10 ?1< 1:?1< 2:?2< 1\n" " 0 \n-1 \n 0 \n",
    "le" ~: testProgramOutput "10 ?1<=1:?1<=2:?2<=1\n" "-1 \n-1 \n 0 \n",
    "gt" ~: testProgramOutput "10 ?1> 1:?1> 2:?2> 1\n" " 0 \n 0 \n-1 \n",
    "ge" ~: testProgramOutput "10 ?1>=1:?1>=2:?2>=1\n" "-1 \n 0 \n-1 \n"
  ]

test_relational_operators_on_strings = TestList $ [
    "eq" ~: testProgramOutput "10 ?\"A\"= \"A\":?\"A\"= \"B\":?\"B\"= \"A\"\n" "-1 \n 0 \n 0 \n",
    "ne" ~: testProgramOutput "10 ?\"A\"<>\"A\":?\"A\"<>\"B\":?\"B\"<>\"A\"\n" " 0 \n-1 \n-1 \n",
    "lt" ~: testProgramOutput "10 ?\"A\"< \"A\":?\"A\"< \"B\":?\"B\"< \"A\"\n" " 0 \n-1 \n 0 \n",
    "le" ~: testProgramOutput "10 ?\"A\"<=\"A\":?\"A\"<=\"B\":?\"B\"<=\"A\"\n" "-1 \n-1 \n 0 \n",
    "gt" ~: testProgramOutput "10 ?\"A\"> \"A\":?\"A\"> \"B\":?\"B\"> \"A\"\n" " 0 \n 0 \n-1 \n",
    "ge" ~: testProgramOutput "10 ?\"A\">=\"A\":?\"A\">=\"B\":?\"B\">=\"A\"\n" "-1 \n 0 \n-1 \n"
  ]

test_boolean_logic = TestList $ [
    "and" ~: testProgramOutput "10 ?0AND0:?0AND-1:?-1AND0:?-1AND-1\n" " 0 \n 0 \n 0 \n-1 \n",
    "or"  ~: testProgramOutput "10 ?0OR 0:?0OR -1:?-1OR 0:?-1OR -1\n" " 0 \n-1 \n-1 \n-1 \n",
    "not" ~: testProgramOutput "10 ?NOT0:?NOT-1:?NOT2\n" "-1 \n 0 \n 0 \n"
  ]

test_boolean_logic_with_nonintegers = TestList $ [
    "and" ~: testProgramOutput "10 ?0.1AND-0.1:?0.1AND0:?0AND0.1\n" " .1 \n 0 \n 0 \n",
    "or"  ~: testProgramOutput "10 ?0OR-0.1:?0.1OR0:?0OR0\n" "-.1 \n .1 \n 0 \n",
    "not" ~: testProgramOutput "10 ?NOT-0.9:?NOT-0.1:?NOT0.1:?NOT0.9:?NOT1.1\n" " 0 \n 0 \n 0 \n 0 \n 0 \n"
  ]

test_negation = testProgramOutput "10 A=5:?-A:?-(-A)\n" "-5 \n 5 \n"

test_abs = testExpressions [
    ("ABS()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("ABS(\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("ABS( 5.7)", " 5.7 "),
    ("ABS( 0  )", " 0 "),
    ("ABS(-5.7)", " 5.7 "),
    ("ABS(1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_asc = testExpressions [
    ("ASC()",            "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("ASC(65)",          "!TYPE MISMATCH IN LINE 1"),
    ("ASC(\"\")",        "!INVALID ARGUMENT IN LINE 1"),
    ("ASC(\"A\")",       " 65 "),
    ("ASC(\"ABC\")",     " 65 "),
    ("ASC(\"0\")",       " 48 "),
    ("ASC(\"0\",\"0\")", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_atn = testExpressions [
    ("ATN()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("ATN(\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("ATN(0)",   " 0 "       ),
    ("ATN(1)",   " .7853982 "),
    ("ATN(10)",  " 1.4711276 "),
    ("ATN(-1)",  "-.7853982 "),
    ("ATN(1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_chr = testExpressions [
    ("CHR$()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("CHR$(\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("CHR$(-1)", "!INVALID ARGUMENT IN LINE 1"),
    ("CHR$(10)", "\n"),
    ("CHR$(13)", "\r"),
    ("CHR$(32)", " "),
    ("CHR$(48)", "0"),
    ("CHR$(65)", "A"),
    ("CHR$(256)", "!INVALID ARGUMENT IN LINE 1"),
    ("CHR$(1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_cos = testExpressions [
    ("COS()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("COS(\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("COS(0)", " 1 "),
    ("COS(1)", " .5403023 "),
    ("COS(1.5707964)", "-4.371139E-8 "),
    ("COS(3.1415927)", "-1 "),
    ("COS(4.712389)",  " 1.1924881E-8 "),
    ("COS(6.2831855)", " 1 "),
    ("COS(1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_exp = testExpressions [
    ("EXP()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("EXP(\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("EXP(-1)", " .36787945 "),
    ("EXP(0)",  " 1 "),
    ("EXP(1)",  " 2.7182817 "),
    ("EXP(2)",  " 7.389056 "),
    ("EXP(1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_int = testExpressions [
    ("INT()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("INT(\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("INT(-23.1)", "-24 "),
    ("INT(23)",    " 23 "),
    ("INT(23.1)",  " 23 "),
    ("INT(23.9)",  " 23 "),
    ("INT(24)",    " 24 "),
    ("INT(1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_left = testExpressions [
    ("LEFT$()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("LEFT$(\"A\")", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("LEFT$(\"ABC\",\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("LEFT$(1,1)", "!TYPE MISMATCH IN LINE 1"),
    ("LEFT$(\"ABC\",-1)", "!INVALID ARGUMENT IN LINE 1"),
    ("LEFT$(\"ABC\",-.1)", "!INVALID ARGUMENT IN LINE 1"),
    ("LEFT$(\"ABC\", 0)", ""),
    ("LEFT$(\"ABC\", 1)", "A"),
    ("LEFT$(\"ABC\", 2)", "AB"),
    ("LEFT$(\"ABC\", 2.9)", "AB"),
    ("LEFT$(\"ABC\", 3)", "ABC"),
    ("LEFT$(\"ABC\", 4)", "ABC"),
    ("LEFT$(\"A\",1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_len = testExpressions [
    ("LEN()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("LEN(1)", "!TYPE MISMATCH IN LINE 1"),
    ("LEN(\"\")",  " 0 "),
    ("LEN(\"A\")", " 1 "),
    ("LEN(\" \" + CHR$(0) + \"23$!\")", " 6 "),
    ("LEN(\"A\", \"B\")", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_log = testExpressions [
    ("LOG()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("LOG(\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("LOG(-1)",        "!INVALID ARGUMENT IN LINE 1"),
    ("LOG( 0)",        "!INVALID ARGUMENT IN LINE 1"),
    ("LOG( 0.1)",      "-2.3025851 "),
    ("LOG( 1)",        " 0 "),
    ("LOG(2.7182817)", " .99999994 "),
    ("LOG(1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_mid = testExpressions [
    ("MID$()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("MID$(\"ABC\")", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("MID$(1,1)", "!TYPE MISMATCH IN LINE 1"),
    ("MID$(\"ABC\",\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("MID$(\"ABC\",-1)", "!INVALID ARGUMENT IN LINE 1"),
    ("MID$(\"ABC\", 0)", "!INVALID ARGUMENT IN LINE 1"),
    ("MID$(\"ABC\", 1)", "ABC"),
    ("MID$(\"ABC\", 2)", "BC"),
    ("MID$(\"ABC\", 3)", "C"),
    ("MID$(\"ABC\", 4)", ""),
    ("MID$(1,1)", "!TYPE MISMATCH IN LINE 1"),
    ("MID$(\"ABC\",\"A\",1)", "!TYPE MISMATCH IN LINE 1"),
    ("MID$(\"ABC\",1,\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("MID$(\"ABC\",-1, 1)", "!INVALID ARGUMENT IN LINE 1"),
    ("MID$(\"ABC\", 0, 1)", "!INVALID ARGUMENT IN LINE 1"),
    ("MID$(\"ABC\", 0,-1)", "!INVALID ARGUMENT IN LINE 1"),
    ("MID$(\"ABC\", 1,0)", ""),
    ("MID$(\"ABC\", 1,1)", "A"),
    ("MID$(\"ABC\", 1,2)", "AB"),
    ("MID$(\"ABC\", 1,3)", "ABC"),
    ("MID$(\"ABC\", 1,4)", "ABC"),
    ("MID$(\"ABC\", 2,0)", ""),
    ("MID$(\"ABC\", 2,1)", "B"),
    ("MID$(\"ABC\", 2,2)", "BC"),
    ("MID$(\"ABC\", 2.9, 2.9)", "BC"),
    ("MID$(\"ABC\", 2,3)", "BC"),
    ("MID$(\"ABC\", 3,0)", ""),
    ("MID$(\"ABC\", 3,1)", "C"),
    ("MID$(\"ABC\", 3,2)", "C"),
    ("MID$(\"ABC\", 4,0)", ""),
    ("MID$(\"ABC\", 4,1)", ""),
    ("MID$(\"ABC\", 1, 1, 1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_right = testExpressions [
    ("RIGHT$()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("RIGHT$(\"A\")", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("RIGHT$(\"ABC\",\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("RIGHT$(1,1)", "!TYPE MISMATCH IN LINE 1"),
    ("RIGHT$(\"ABC\",-1)", "!INVALID ARGUMENT IN LINE 1"),
    ("RIGHT$(\"ABC\", 0)", ""),
    ("RIGHT$(\"ABC\", 1)", "C"),
    ("RIGHT$(\"ABC\", 2)", "BC"),
    ("RIGHT$(\"ABC\", 2.9)", "BC"),
    ("RIGHT$(\"ABC\", 3)", "ABC"),
    ("RIGHT$(\"ABC\", 4)", "ABC"),
    ("RIGHT$(\"A\",1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_rnd = testProgramOutput
    "1 ?RND(-1):?RND(1):?RND(1):?RND(0):?RND(10):?RND(-2.5)\n"
    " .72774446 \n .6806801 \n .2628854 \n .2628854 \n .8841321 \n .2695619 \n"

test_rnd_errors = testExpressions [
    ("RND()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("RND(\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("RND(1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

-- RANDOMIZE is difficult to test because it depends on system time

test_sgn = testExpressions [
    ("SGN()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("SGN(\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("SGN( 5.7)", " 1 "),
    ("SGN( 0  )", " 0 "),
    ("SGN(-5.7)", "-1 "),
    ("SGN(1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_sin = testExpressions [
    ("SIN()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("SIN(\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("SIN(0)", " 0 "),
    ("SIN(1)", " .84147096 "),
    ("SIN(1.5707964)", " 1 "),
    ("SIN(3.1415927)", "-8.742278E-8 "),
    ("SIN(4.712389)",  "-1 "),
    ("SIN(6.2831855)", " 1.7484555E-7 "),
    ("SIN(1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_spc = testExpressions [
    ("SPC()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("SPC(\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("SPC(-1)", "!INVALID ARGUMENT IN LINE 1"),
    ("SPC(0)", ""),
    ("SPC(1)", " "),
    ("SPC(2.9)", "  "),
    ("SPC(10)", "          "),
    ("SPC(1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_sqr = testExpressions [
    ("SQR()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("SQR(\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("SQR(-1)", "!INVALID ARGUMENT IN LINE 1"),
    ("SQR(0)", " 0 "),
    ("SQR(2)", " 1.4142135 "),
    ("SQR(4)", " 2 "),
    ("SQR(1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_str = testExpressions [
    ("STR$()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("STR$(\"ABC\")", "!TYPE MISMATCH IN LINE 1"),
    ("STR$(1E2)", " 100"),
    ("STR$(.000000001)", " 1.E-9"),
    ("STR$(1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_tan = testExpressions [
    ("TAN()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("TAN(\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("TAN(0)", " 0 "),
    ("TAN(.7853982)", " 1 "),
    ("TAN(1)", " 1.5574077 "),
    ("TAN(1.5707964)", "-22877332 "),
    ("TAN(3.1415927)", " 8.742278E-8 "),
    ("TAN(4.712389)",  "-83858280 "),
    ("TAN(6.2831855)", " 1.7484555E-7 "),
    ("TAN(1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_val = testExpressions [
    ("VAL()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("VAL(1)", "!TYPE MISMATCH IN LINE 1"),
    ("VAL(\"\")", " 0 "),
    ("VAL(\"123\")", " 123 "),
    ("VAL(\" 123 \")", " 123 "),
    ("VAL(\" 1 2\")", " 1 "),
    ("VAL(\"1A\")", " 1 "),
    ("VAL(\"A\")", " 0 "),
    ("VAL(\"1\",\"1\")", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_goto_forwards = testProgramOutput "10GOTO30\n20?20\n30?30\n" " 30 \n"

test_goto_backwards = testProgramOutput "10GOTO50\n20?20\n30?30\n40END\n50GOTO30\n" " 30 \n"

test_gosub = testProgramOutput "1?1\n2GOSUB5\n3?3\n4END\n5?5\n6RETURN\n" " 1 \n 5 \n 3 \n"

test_nested_gosub = testProgramOutput (unlines [
    "  1 ?1     ",
    "  2 GOSUB5 ",
    "  3 ?3     ",
    "  4 END    ",
    "  5 ?5     ",
    "  6 GOSUB9 ",
    "  7 ?7     ",
    "  8 RETURN ",
    "  9 ?9     ",
    " 10 RETURN "
  ])
  " 1 \n 5 \n 9 \n 7 \n 3 \n"

test_return_does_double_duty = testProgramOutput (unlines [
    "10 ?10:GOSUB30",
    "20 ?20:END",
    "30 ?30:GOSUB40",
    "40 ?40:RETURN"
  ])
  " 10 \n 30 \n 40 \n 40 \n 20 \n"

test_on_goto = TestList [
    testProgramOutput "1 ON\"A\"GOTO2\n" "!TYPE MISMATCH IN LINE 1\n",
    testProgramOutput "1 A=-1  :ONAGOTO3,4,5\n2 ?2\n3 ?3\n4 ?4\n5 ?5\n6 ?6\n" " 2 \n 3 \n 4 \n 5 \n 6 \n",
    testProgramOutput "1 A=-0.1:ONAGOTO3,4,5\n2 ?2\n3 ?3\n4 ?4\n5 ?5\n6 ?6\n" " 2 \n 3 \n 4 \n 5 \n 6 \n",
    testProgramOutput "1 A= 0  :ONAGOTO3,4,5\n2 ?2\n3 ?3\n4 ?4\n5 ?5\n6 ?6\n" " 2 \n 3 \n 4 \n 5 \n 6 \n",
    testProgramOutput "1 A= 1  :ONAGOTO3,4,5\n2 ?2\n3 ?3\n4 ?4\n5 ?5\n6 ?6\n" " 3 \n 4 \n 5 \n 6 \n",
    testProgramOutput "1 A= 2  :ONAGOTO3,4,5\n2 ?2\n3 ?3\n4 ?4\n5 ?5\n6 ?6\n" " 4 \n 5 \n 6 \n",
    testProgramOutput "1 A= 2.9:ONAGOTO3,4,5\n2 ?2\n3 ?3\n4 ?4\n5 ?5\n6 ?6\n" " 4 \n 5 \n 6 \n",
    testProgramOutput "1 A= 3  :ONAGOTO3,4,5\n2 ?2\n3 ?3\n4 ?4\n5 ?5\n6 ?6\n" " 5 \n 6 \n",
    testProgramOutput "1 A= 4  :ONAGOTO3,4,5\n2 ?2\n3 ?3\n4 ?4\n5 ?5\n6 ?6\n" " 2 \n 3 \n 4 \n 5 \n 6 \n",
    testProgramOutput "1 A= 2  :ONAGOTO3,5,4\n2 ?2\n3 ?3\n4 ?4\n5 ?5\n6 ?6\n" " 5 \n 6 \n"
  ]

test_on_gosub = TestList [
    testProgramOutput "1 ON\"A\"GOSUB2\n" "!TYPE MISMATCH IN LINE 1\n",
    testProgramOutput "1 A=-1  :ONAGOSUB3,4,5:END\n2 ?2:RETURN\n3 ?3:RETURN\n4 ?4:RETURN\n5 ?5:RETURN\n6 ?6:RETURN\n" "",
    testProgramOutput "1 A=-0.1:ONAGOSUB3,4,5:END\n2 ?2:RETURN\n3 ?3:RETURN\n4 ?4:RETURN\n5 ?5:RETURN\n6 ?6:RETURN\n" "",
    testProgramOutput "1 A= 0  :ONAGOSUB3,4,5:END\n2 ?2:RETURN\n3 ?3:RETURN\n4 ?4:RETURN\n5 ?5:RETURN\n6 ?6:RETURN\n" "",
    testProgramOutput "1 A= 1  :ONAGOSUB3,4,5:END\n2 ?2:RETURN\n3 ?3:RETURN\n4 ?4:RETURN\n5 ?5:RETURN\n6 ?6:RETURN\n" " 3 \n",
    testProgramOutput "1 A= 2  :ONAGOSUB3,4,5:END\n2 ?2:RETURN\n3 ?3:RETURN\n4 ?4:RETURN\n5 ?5:RETURN\n6 ?6:RETURN\n" " 4 \n",
    testProgramOutput "1 A= 2.9:ONAGOSUB3,4,5:END\n2 ?2:RETURN\n3 ?3:RETURN\n4 ?4:RETURN\n5 ?5:RETURN\n6 ?6:RETURN\n" " 4 \n",
    testProgramOutput "1 A= 3  :ONAGOSUB3,4,5:END\n2 ?2:RETURN\n3 ?3:RETURN\n4 ?4:RETURN\n5 ?5:RETURN\n6 ?6:RETURN\n" " 5 \n",
    testProgramOutput "1 A= 4  :ONAGOSUB3,4,5:END\n2 ?2:RETURN\n3 ?3:RETURN\n4 ?4:RETURN\n5 ?5:RETURN\n6 ?6:RETURN\n" "",
    testProgramOutput "1 A= 2  :ONAGOSUB3,5,4:END\n2 ?2:RETURN\n3 ?3:RETURN\n4 ?4:RETURN\n5 ?5:RETURN\n6 ?6:RETURN\n" " 5 \n"
  ]

test_if = TestList [
    testProgramOutput "1 IF\"A\"THEN?1\n" "!TYPE MISMATCH IN LINE 1\n",
    testProgramOutput "1 IF-11THEN?1:?2\n2 ?3\n" " 1 \n 2 \n 3 \n",
    testProgramOutput "1 IF 0THEN?1:?2\n2 ?3\n" " 3 \n",
    testProgramOutput "1 IF 1THEN?1:?2\n2 ?3\n" " 1 \n 2 \n 3 \n",
    testProgramOutput "1 IF .1THEN?1:?2\n2 ?3\n" " 1 \n 2 \n 3 \n",
    testProgramOutput "1 IF -.1THEN?1:?2\n2 ?3\n" " 1 \n 2 \n 3 \n",
    testProgramOutput "1 IF 2.1THEN?1:?2\n2 ?3\n" " 1 \n 2 \n 3 \n"
  ]

test_for = TestList [
    testProgramOutput "1 FORA$=1TO10\n" "!TYPE MISMATCH IN LINE 1\n",
    testProgramOutput "1 FORA%=1TO10\n" "!TYPE MISMATCH IN LINE 1\n",
    testProgramOutput "1 FORA=\"A\"TO3\n" "!TYPE MISMATCH IN LINE 1\n",
    testProgramOutput "1 FORA=1TO\"A\"\n" "!TYPE MISMATCH IN LINE 1\n",
    testProgramOutput "1 FORA=1TO3:?A:NEXT:?99:?A\n" " 1 \n 2 \n 3 \n 99 \n 4 \n",
    testProgramOutput "1 FORA=2TO8STEP2:?A:NEXT:?99:?A\n" " 2 \n 4 \n 6 \n 8 \n 99 \n 10 \n",
    testProgramOutput "1 FORA=8TO2STEP-2:?A:NEXT:?99:?A\n" " 8 \n 6 \n 4 \n 2 \n 99 \n 0 \n",
    testProgramOutput "1 FORA=1TO0:?A:NEXT:?99:?A\n" " 1 \n 99 \n 2 \n",
    testProgramOutput "1 FORA=1TO3\n2 ?A\n3 NEXT\n" " 1 \n 2 \n 3 \n",
    testProgramOutput "1 FORA=1TO3\n2 ?A\n3 NEXTA\n" " 1 \n 2 \n 3 \n",
    testProgramOutput "1 FORA=1TO3\n2 ?A\n3 NEXTB\n" " 1 \n!NEXT WITHOUT FOR ERROR (VAR B) IN LINE 3\n"
  ]

test_nested_for = testProgramOutput (unlines [
    "1 FORA=10TO20STEP10",
    "2 FORB=1TO3",
    "3 ?A+B",
    "4 NEXT",
    "5 NEXT"
  ])
  " 11 \n 12 \n 13 \n 21 \n 22 \n 23 \n"

test_improperly_nested_for = testProgramOutput (unlines [
    "1 FORA=10TO20STEP10",
    "2 FORB=1TO3",
    "3 ?A+B",
    "4 NEXTA",
    "5 NEXTB"
  ])
  " 11 \n 21 \n 32 \n!NEXT WITHOUT FOR ERROR (VAR A) IN LINE 4\n"

test_return_in_for = testProgramOutput (unlines [
    "10 GOSUB40",
    "20 NEXT",
    "30 END",
    "40 FORI=1TO3",
    "50 PRINT\"HELLO\"",
    "60 RETURN"
  ])
  "HELLO\nHELLO\n!RETURN WITHOUT GOSUB ERROR IN LINE 60\n"

test_print_literals = testProgramOutput "10 PRINT\"NUMBER\";5;\"AND\";-2\n" "NUMBER 5 AND-2 \n"

test_float_representation = testProgramOutput "10 ?1E17:?5.55555\n" " 1.E+17 \n 5.55555 \n"

test_tab = testProgramOutput
    "10 ?TAB(4);\"A\";TAB(10);\"B\";TAB(11);\"C\";TAB(4);\"D\"\n"
    "    A     BCD\n"

test_tab_with_nonintegers = testProgramOutput "10 ?TAB(2.9);\"A\"\n" "  A\n"

test_tab_with_newlines_and_linefeeds = testProgramOutput
    "10 ?\"HI\";CHR$(13);\"TO\";TAB(5);\"YOU\";CHR$(10);\"AND\";TAB(10);\"ME\"\n"
    "HI\rTO   YOU\nAND       ME\n"

test_tab_wrongargs = testExpressions [
    ("TAB()", "!WRONG NUMBER OF ARGUMENTS IN LINE 1"),
    ("TAB(\"A\")", "!TYPE MISMATCH IN LINE 1"),
    ("TAB(-1)", "!INVALID ARGUMENT IN LINE 1"),
    ("TAB(1,1)", "!WRONG NUMBER OF ARGUMENTS IN LINE 1")
  ]

test_print_zones = testProgramOutput
    "1 ?\"A\",\"B\",,\"C\":?\"123456789012345\",\"X\":?1,;:?2,:?,3\n"
    "A             B                           C\n123456789012345             X\n 1             2                           3 \n"

test_input = TestList [
    "multiple values and types" ~: testProgramOutputWithInput
        "1 INPUT\"WHAT\";A$,B,CD$(9),M%\n2 ?:?A$:?B:?CD$(9):?M%\n"
        "  Bob 4 , 3  ,2  1, 5.2\n"
        "WHAT? \nBob 4\n 3 \n2  1\n 5 \n",
    "trims whitespace" ~: testProgramOutputWithInput
        "1 INPUTA$:?:?A$\n"
        "  \t   Bob 4 \n"
        "? \nBob 4\n",
    "quoted strings" ~: testProgramOutputWithInput
        "1 INPUTA$\n2 ?:?A$\n"
        "  \t\"   Bob 4 \"         \t   \n"
        "? \n   Bob 4 \n",
    "re-prompts for invalid number" ~: testProgramOutputWithInput
        "1 INPUTA:?:?A\n"
        "X\n4\n"
        "? !NUMBER EXPECTED - RETRY INPUT LINE\n? \n 4 \n",
    "re-prompts if not enough data on line" ~: testProgramOutputWithInput
        "1 INPUT\"NUM\";A,B:?:?A:?B\n"
        "3\n4\n"
        "NUM? ?? \n 3 \n 4 \n",
    "extra input ignored" ~: testProgramOutputWithInput
        "1 INPUTA\n"
        "1,2\n"
        "? !EXTRA INPUT IGNORED\n",
    "error if we reach eof without enough data" ~: testProgramOutputWithInput
        "1 INPUTA,B:?:?A:?B\n"
        "4\n"
        "? ?? !END OF INPUT IN LINE 1\n",
    "error if we reach eof without enough data (no newline)" ~: testProgramOutputWithInput
        "1 INPUTA:?:?A\n"
        ""
        "? !END OF INPUT IN LINE 1\n",
    "input resets tab location" ~: testProgramOutputWithInput
        "1 INPUT\"HELLO\";A$:?TAB(3);\"X\"\n"
        "GOODBYE\n"
        "HELLO?    X\n"
  ]

test_read_data = TestList [
    "multiple values and types" ~: testProgramOutput
        "1 READ A$,B,CD$(9),M%\n2 ?A$:?B:?CD$(9):?M%\n3 DATA  Bob 4 , 3  ,2  1, 5.2\n"
        "Bob 4\n 3 \n2  1\n 5 \n",
    "trims whitespace" ~: testProgramOutput
        "1 READA$:?A$\n2 DATA  \t   Bob 4 \n"
        "Bob 4\n",
    "quoted strings" ~: testProgramOutput
        "1 READA$\n2 ?A$\n3 DATA  \t\"   Bob 4 \"         \t   \n"
        "   Bob 4 \n",
    "type mismatch for invalid number" ~: testProgramOutput
        "1 READA:?:?A\n2 DATAX\n"
        "!TYPE MISMATCH IN LINE 1\n",
    "data on multiple lines" ~: testProgramOutput
        "1 READA,B:?A:?B\n2 DATA3\n3 DATA4\n"
        " 3 \n 4 \n",
    "extra data ignored" ~: testProgramOutput
        "1 READA:?A\n2 DATA1,2\n"
        " 1 \n",
    "error if we reach eof without enough data" ~: testProgramOutput
        "1 READA,B:?A:?B\n2 DATA4\n"
        "!OUT OF DATA IN LINE 1\n",
    "don't care where data appears" ~: testProgramOutput
        (unlines [
            "1 DATA A:?7",
            "2 READA$,B$,C:DATAmore",
            "3 DATA 5",
            "4 ?A$:?B$:?C"
        ])
        "A:?7\nmore\n 5 \n"
  ]

test_restore = testProgramOutput
    (unlines [
        "1 READ A,B,C:RESTORE 3:READ D,E:RESTORE:READ F,G,H:?A:?B:?C:?D:?E:?F:?G:?H\n",
        "2 DATA1",
        "3 DATA2",
        "4 DATA3"
    ])
    " 1 \n 2 \n 3 \n 2 \n 3 \n 1 \n 2 \n 3 \n"

test_def_fn = TestList [
    "simple float" ~: testProgramOutput
        "1 DEFFNA(X)=X^2+1:?FNA(4)\n"
        " 17 \n",
    "multi-arg float" ~: testProgramOutput
        "1 DEFFNA(X,Y,Z$)=LEN(Z$)^2+2*X+Y:?FNA(2,3,\"HELLO\")\n"
        " 32 \n",
    "multi-arg string" ~: testProgramOutput
        "1 DEFFNA$(X$,Y,Z)=MID$(X$,Y-1,Z+1):?FNA$(\"MAGILLICUDDY\",3,4)\n"
        "AGILL\n",
    "wrong num args" ~: testProgramOutput
        "1 DEFFNA(X)=X*2:?FNA()\n"
        "!WRONG NUMBER OF ARGUMENTS IN LINE 1\n",
    "type mismatch" ~: testProgramOutput
        "1 DEFFNA(X)=X*2:?A(\"X\")\n"
        "!TYPE MISMATCH IN LINE 1\n",
    "undefined" ~: testProgramOutput
        "1 ?FNA(5):DEFFNA(X)=X*2\n"
        "!UNDEFINED FUNCTION A IN LINE 1\n"
  ]

test_stop = testProgramOutput "1 ?1:STOP:?2\n" " 1 \n"

test_rem = testProgramOutput "1 ?1:REM COMMENT:?2\n2 ?3\n" " 1 \n 3 \n"
