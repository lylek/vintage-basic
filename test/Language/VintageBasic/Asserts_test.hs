module Language.VintageBasic.Asserts_test where

import Test.HUnit
import Language.VintageBasic.Asserts

test_assertIOError = TestCase $ do
    assertIOError (fail "blah") "blah"
