{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}

-- | A class describing IO operations with instances for IO and strings.
-- This is primarily for the purpose of unit testing.
module IO.IOStream (IOStream(..),IOStream'(..)) where

import Data.IORef
import System.IO

class IOStream' h where
    vGetContents :: h -> IO String
    vSetContents :: h -> String -> IO ()
    vPutStr :: h -> String -> IO ()
    vFlush :: h -> IO ()
    vGetLine :: h -> IO String
    vIsEOF :: h -> IO Bool

instance IOStream' Handle where
    vGetContents = hGetContents
    vSetContents h s = hSetFileSize h 0 >> hPutStr h s
    vPutStr  = hPutStr
    vFlush   = hFlush
    vGetLine = hGetLine
    vIsEOF   = hIsEOF

instance IOStream' (IORef String) where
    vGetContents h = readIORef h
    vSetContents h s = writeIORef h s
    vPutStr h s = modifyIORef h (\text -> text ++ s)
    vFlush _ = return ()
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

data IOStream = forall h. IOStream' h => IOStream h

instance IOStream' IOStream where
    vGetContents (IOStream h) = vGetContents h
    vSetContents (IOStream h) = vSetContents h
    vPutStr (IOStream h) = vPutStr h
    vFlush (IOStream h) = vFlush h
    vGetLine (IOStream h) = vGetLine h
    vIsEOF (IOStream h) = vIsEOF h
