module BasicTokenizer_test where

import Test.HUnit
import BasicAsserts
import BasicBuiltin(Builtin(..))
import BasicLineScanner
import BasicTokenizer(Token(..),taggedTokensP,printToken)

test_taggedTokensP = TestCase $ do
  let source = [
                ",:;()$%=<><=>=><+-*/^.?",
                "ABSASCCHR$COSEXPINTLEFT$LENLOGMID$RIGHT$RNDSGNSPCSQRSTR$TABTANVAL",
                "ANDORNOTLETDIMONGOSUBRETURNIFTHENFORTOSTEPNEXTPRINTINPUT",
                "RANDOMIZEREADRESTOREFNEND",
                "\"hello\"REMGOFORTH\"ANDREAD\"",
                "XDATATODATA"
               ]
  let expectedLenAndTokss = [
                        [(1,CommaTok), (1,ColonTok), (1,SemiTok), (1,LParenTok), (1,RParenTok),
                         (1,DollarTok), (1,PercentTok), (1,EqTok), (2,NETok), (2,LETok), (2,GETok),
                         (1,GTTok), (1,LTTok), (1,PlusTok), (1,MinusTok), (1,MulTok), (1,DivTok),
                         (1,PowTok), (1,DotTok), (1,PrintTok)],
                        [(3,BuiltinTok AbsBI), (3,BuiltinTok AscBI), (4,BuiltinTok ChrBI),
                         (3,BuiltinTok CosBI), (3,BuiltinTok ExpBI), (3,BuiltinTok IntBI),
                         (5,BuiltinTok LeftBI), (3,BuiltinTok LenBI), (3,BuiltinTok LogBI),
                         (4,BuiltinTok MidBI), (6,BuiltinTok RightBI), (3,BuiltinTok RndBI),
                         (3,BuiltinTok SgnBI), (3,BuiltinTok SpcBI), (3,BuiltinTok SqrBI),
                         (4,BuiltinTok StrBI), (3,BuiltinTok TabBI), (3,BuiltinTok TanBI),
                         (3,BuiltinTok ValBI)],
                        [(3,AndTok), (2,OrTok), (3,NotTok), (3,LetTok), (3,DimTok), (2,OnTok),
                         (2,GoTok), (3,SubTok), (6,ReturnTok), (2,IfTok), (4,ThenTok), (3,ForTok),
                         (2,ToTok), (4,StepTok), (4,NextTok), (5,PrintTok), (5,InputTok)],
                        [(9,RandomizeTok), (4,ReadTok), (7,RestoreTok), (2,FnTok), (3,EndTok)],
                        [(7,StringTok "hello"), (19,RemTok "GOFORTH\"ANDREAD\"")],
                        [(1,CharTok 'X'), (10,DataTok "TODATA")]
                       ]
  let accumLensWToks lenAndToks =
          let (lens, toks) = unzip lenAndToks
              cols = scanl (+) 1 lens
              in zip cols toks
  let expectedColAndTokss = map accumLensWToks expectedLenAndTokss
  sequence_ $ zipWith (assertColAndParseResult taggedTokensP) source expectedColAndTokss

test_capitalizes_lowercase_chars = TestCase $ do
   let source = "azAZ"
   let expected = [(1,CharTok 'A'), (2,CharTok 'Z'), (3,CharTok 'A'), (4, CharTok 'Z')]
   assertColAndParseResult taggedTokensP source expected

test_eats_spaces_after_most_tokens_but_not_chars = TestCase $ do
   let source = "   ,   +  AND  ORX   YZ  "
   let expectedColAndToks = [(1,SpaceTok), (4,CommaTok), (8,PlusTok), (11,AndTok), (16,OrTok),
                             (18,CharTok 'X'), (19,SpaceTok), (22,CharTok 'Y'), (23,CharTok 'Z'),
                             (24,SpaceTok)]
   assertColAndParseResult taggedTokensP source expectedColAndToks

test_reports_error_for_an_illegal_char = TestCase $ do
   sequence_ [ assertParseError taggedTokensP [illegalChar] "expecting legal BASIC character"
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
