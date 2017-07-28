module Language.VintageBasic.Tokenizer_test where

import Data.Char(toLower)
import Test.HUnit
import Language.VintageBasic.Asserts
import Language.VintageBasic.Builtins(Builtin(..))
import Language.VintageBasic.Result
import Language.VintageBasic.LineScanner
import Language.VintageBasic.Tokenizer(Token(..),taggedTokensP,printToken)

assertTokenizerResult = assertParseResult withCol SyntaxError taggedTokensP
assertTokenizerError  = assertParseError  withCol SyntaxError taggedTokensP

tokenLengthsToPositions :: [(Int, Token)] -> [(Int, Token)]
tokenLengthsToPositions lenAndToks =
  let (lens, toks) = unzip lenAndToks
      cols = scanl (+) 1 lens
  in zip cols toks

test_tokenizes_punctuation = TestCase $ do
  let source = ",:;()$%=<><=>=><+-*/^.?"
  let expected = tokenLengthsToPositions
                 [(1,CommaTok), (1,ColonTok), (1,SemiTok), (1,LParenTok), (1,RParenTok),
                 (1,DollarTok), (1,PercentTok), (1,EqTok), (2,NETok), (2,LETok), (2,GETok),
                 (1,GTTok), (1,LTTok), (1,PlusTok), (1,MinusTok), (1,MulTok), (1,DivTok),
                 (1,PowTok), (1,DotTok), (1,PrintTok)]
  assertTokenizerResult source expected

test_tokenizes_builtins = TestCase $ do
  let ucSource = "ABSASCCHR$COSEXPINTLEFT$LENLOGMID$RIGHT$RNDSGNSINSPCSQRSTR$TABTANVAL"
  let lcSource = map toLower ucSource
  let expected =  tokenLengthsToPositions
                  [(3,BuiltinTok AbsBI), (3,BuiltinTok AscBI), (4,BuiltinTok ChrBI),
                  (3,BuiltinTok CosBI), (3,BuiltinTok ExpBI), (3,BuiltinTok IntBI),
                  (5,BuiltinTok LeftBI), (3,BuiltinTok LenBI), (3,BuiltinTok LogBI),
                  (4,BuiltinTok MidBI), (6,BuiltinTok RightBI), (3,BuiltinTok RndBI),
                  (3,BuiltinTok SgnBI), (3,BuiltinTok SinBI), (3,BuiltinTok SpcBI),
                  (3,BuiltinTok SqrBI), (4,BuiltinTok StrBI), (3,BuiltinTok TabBI),
                  (3,BuiltinTok TanBI), (3,BuiltinTok ValBI)]
  assertTokenizerResult ucSource expected
  assertTokenizerResult lcSource expected

test_tokenizes_other = TestCase $ do
  let ucSource = "ANDORNOTLETDIMONGOSUBRETURNIFTHENFORTOSTEPNEXTPRINTINPUT"
        ++ "RANDOMIZEREADRESTOREDEFFNENDSTOP"
  let lcSource = map toLower ucSource
  let expected = tokenLengthsToPositions
                 [(3,AndTok), (2,OrTok), (3,NotTok), (3,LetTok), (3,DimTok), (2,OnTok),
                 (2,GoTok), (3,SubTok), (6,ReturnTok), (2,IfTok), (4,ThenTok), (3,ForTok),
                 (2,ToTok), (4,StepTok), (4,NextTok), (5,PrintTok), (5,InputTok),
                 (9,RandomizeTok), (4,ReadTok), (7,RestoreTok), (3,DefTok), (2,FnTok),
                 (3,EndTok), (4,StopTok)]
  assertTokenizerResult ucSource expected
  assertTokenizerResult lcSource expected

test_tokenizes_string_and_rem = TestCase $ do
  let ucSource = "\"HELLO\"REMGOFORTH\"ANDREAD\""
  let lcSource = map toLower ucSource
  let ucExpected = tokenLengthsToPositions [(7,StringTok "HELLO"), (19,RemTok "GOFORTH\"ANDREAD\"")]
  let lcExpected = tokenLengthsToPositions [(7,StringTok "hello"), (19,RemTok "goforth\"andread\"")]
  assertTokenizerResult ucSource ucExpected
  assertTokenizerResult lcSource lcExpected

test_tokenizes_plain_char_and_data = TestCase $ do
  let ucSource = "XDATATODATA"
  let lcSource = map toLower ucSource
  let ucExpected = tokenLengthsToPositions [(1,CharTok 'X'), (10,DataTok "TODATA")]
  let lcExpected = tokenLengthsToPositions [(1,CharTok 'X'), (10,DataTok "todata")]
  assertTokenizerResult ucSource ucExpected
  assertTokenizerResult lcSource lcExpected

test_capitalizes_lowercase_chars = TestCase $ do
   let source = "azAZ"
   let expected = [(1,CharTok 'A'), (2,CharTok 'Z'), (3,CharTok 'A'), (4, CharTok 'Z')]
   assertTokenizerResult source expected

test_eats_spaces_after_most_tokens_but_not_chars = TestCase $ do
   let source = "   ,   +  AND  ORX   YZ  "
   let expectedColAndToks = [(1,SpaceTok), (4,CommaTok), (8,PlusTok), (11,AndTok), (16,OrTok),
                             (18,CharTok 'X'), (19,SpaceTok), (22,CharTok 'Y'), (23,CharTok 'Z'),
                             (24,SpaceTok)]
   assertTokenizerResult source expectedColAndToks

test_reports_error_for_an_illegal_char = TestCase $ do
   sequence_ [ assertTokenizerError [illegalChar] "EXPECTING LEGAL BASIC CHARACTER"
               | illegalChar <- "~`!@#&_[]{}\\'\n\a" ]

test_printToken = TestCase $ do
   let tokens = [CommaTok, ColonTok, SemiTok, LParenTok, RParenTok, DollarTok, PercentTok, EqTok,
                 NETok, GETok, GTTok, LTTok, PlusTok, MinusTok, MulTok, DivTok, PowTok, AndTok,
                 OrTok, NotTok, LetTok, DimTok, OnTok, GoTok, SubTok, ReturnTok, IfTok, ThenTok,
                 ForTok, ToTok, StepTok, NextTok, PrintTok, InputTok, RandomizeTok, ReadTok,
                 RestoreTok, FnTok, EndTok, StringTok "hello", SpaceTok, RemTok "comment \"here",
                 CharTok 'X', DataTok "DATA,More Data   ,5"]
   let expectedStrings = [",", ":", ";", "(", ")", "$", "%", "=", "<>", ">=", ">", "<", "+", "-",
                          "*", "/", "^", "AND", "OR", "NOT", "LET", "DIM", "ON", "GO", "SUB",
                          "RETURN", "IF", "THEN", "FOR", "TO", "STEP", "NEXT", "PRINT", "INPUT",
                          "RANDOMIZE", "READ", "RESTORE", "FN", "END", "\"hello\"", " ",
                          "REMcomment \"here", "X", "DATADATA,More Data   ,5"]
   assertEqual "" expectedStrings (map printToken tokens)
