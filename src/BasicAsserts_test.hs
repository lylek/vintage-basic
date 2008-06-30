module BasicAsserts_test where

import Test.HUnit
import BasicAsserts

test_assertIOError = TestCase $ do
    assertIOError (fail "blah") "blah"
