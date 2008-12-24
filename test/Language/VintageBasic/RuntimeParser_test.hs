module Language.VintageBasic.RuntimeParser_test where

import Test.HUnit
import Text.ParserCombinators.Parsec
import Language.VintageBasic.RuntimeParser(dataValsP,readFloat)

floatTest string expected = assertEqual "" expected (readFloat string)

test_readFloat = TestCase $
    mapM_ (uncurry floatTest) [
        ("A",     Nothing  ),
        (".",     Just   0.0 ),
        ("0",     Just   0.0 ),
        ("1",     Just   1.0 ),
        (" 1",    Nothing    ),
        ("-1",    Just (-1.0)),
        ("+1",    Just   1.0 ),
        ("1.",    Just   1.0 ),
        ("-1.",   Just (-1.0)),
        (".1",    Just   0.1 ),
        ("-.1",   Just (-0.1)),
        ("1.0",   Just   1.0 ),
        ("1E",    Nothing    ),
        ("1E.2",  Nothing    ),
        ("1A",    Just   1.0 ),
        ("1 E2",  Just   1.0 ),
        ("1 2",   Just   1.0 ),
        ("1E2",   Just   1.0E+2 ),
        ("1E+2",  Just   1.0E+2 ),
        ("1E-2",  Just   1.0E-2 ),
        ("-1E-2", Just (-1.0E-2)),
        ("123456789", Just 123456789.0)
    ]
