{-# LANGUAGE FlexibleContexts #-}

module Language.VintageBasic.FloatParser_test where

import Test.HUnit
import Language.VintageBasic.Asserts
import Language.VintageBasic.FloatParser
import Language.VintageBasic.Result

assertFloatPSuccess source expected = assertParseResult id ScanError floatP source expected
assertFloatPFailure source expected = assertParseError  id ScanError floatP source expected

test_successes = TestCase $ do
    mapM_ (uncurry assertFloatPSuccess) success_cases

success_cases =
  [
    ( "0",           0     ),
    ( ".",           0     ),
    ( ".25",         0.25  ),
    ( "123",       123     ),
    ( "12.",        12     ),
    ( "-1",         -1     ),
    ( "+12",        12     ),
    ( ".5",          0.5   ),
    ( "2.5",         2.5   ),
    ( "1e2",       100     ),
    ( "1E2",       100     ),
    ( "2.5E-1",      0.25  ),
    ( "+12.5e-2",    0.125 ),
    ( "+12.5e+2", 1250     )
  ]

test_failures = TestCase $ do
    mapM_ (`assertFloatPFailure` "") failure_cases

failure_cases =
  [
    "/",
    "+",
    "-",
    "E",
    "E10",
    "A",
    "1E",
    "1A"
  ]
