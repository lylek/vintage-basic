{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}

-- | A class describing IO operations with instances for IO and strings.
-- This is primarily for the purpose of unit testing.
module IO.IOStream (IOStream(..),IOStream'(..)) where

import Data.IORef
import System.IO

-- | The abstract base class describing methods for IOStreams.
-- The purpose of each function is identical to the ones in
-- System.IO.Handle starting with an 'h' instead of a 'v'.
class IOStream' h where
    vGetContents :: h -> IO String
    vSetContents :: h -> String -> IO ()
    vPutStr :: h -> String -> IO ()
    vFlush :: h -> IO ()
    vGetChar :: h -> IO Char
    vGetLine :: h -> IO String
    vIsEOF :: h -> IO Bool

-- | An instance of IOStream' for IO Handles.
instance IOStream' Handle where
    vGetContents     = hGetContents
    vSetContents h s = hSetFileSize h 0 >> hPutStr h s
    vPutStr          = hPutStr
    vFlush           = hFlush
    vGetChar h       = hSetBuffering h NoBuffering >> hGetChar h
    vGetLine h       = hSetBuffering h LineBuffering >> hGetLine h
    vIsEOF           = hIsEOF

-- | An instance for IOStream' for references to Strings.
instance IOStream' (IORef String) where
    vGetContents h = readIORef h
    vSetContents h s = writeIORef h s
    vPutStr h s = modifyIORef h (\text -> text ++ s)
    vFlush _ = return ()
    vGetChar h = do
        text <- readIORef h
        let (c:text') = text
        writeIORef h text'
        return c
    vGetLine h = do
        text <- readIORef h
        let (s, text') = span (/= '\n') text
        if not (null text') && head text' == '\n'
            then do
                writeIORef h (tail text')
                return s
            else do
                writeIORef h ""
                return text'
    vIsEOF h = do
        text <- readIORef h
        return $ null text

-- | A datatype wrapper for IOStream'.
data IOStream = forall h. IOStream' h => IOStream h

-- | IOStream is itself an instance of IOStream', which reduces the
-- overhead of unwrapping them from the IOStream constructor.
instance IOStream' IOStream where
    vGetContents (IOStream h) = vGetContents h
    vSetContents (IOStream h) = vSetContents h
    vPutStr (IOStream h) = vPutStr h
    vFlush (IOStream h) = vFlush h
    vGetChar (IOStream h) = vGetChar h
    vGetLine (IOStream h) = vGetLine h
    vIsEOF (IOStream h) = vIsEOF h
