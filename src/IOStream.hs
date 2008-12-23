{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}

-- | A class describing IO operations with instances for IO and strings.
-- This is primarily for the purpose of unit testing.
module IOStream (IOStream(..),IOStream'(..)) where

import Data.IORef
import System.IO
import qualified Data.ByteString.Char8 as BS

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

instance IOStream' (IORef BS.ByteString) where
    vGetContents h = do
        text <- readIORef h
        return $ BS.unpack text
    vSetContents h s = writeIORef h (BS.pack s)
    vPutStr h s = modifyIORef h (\text -> BS.append text (BS.pack s))
    vFlush _ = return ()
    vGetLine h = do
        text <- readIORef h
        let (s, text') = BS.span (/= '\n') text
        if not (BS.null text') && BS.head text' == '\n'
            then do
                writeIORef h (BS.tail text')
                return (BS.unpack s)
            else do
                writeIORef h BS.empty
                return (BS.unpack text')
    vIsEOF h = do
        text <- readIORef h
        return $ BS.null text

data IOStream = forall h. IOStream' h => IOStream h

instance IOStream' IOStream where
    vGetContents (IOStream h) = vGetContents h
    vSetContents (IOStream h) = vSetContents h
    vPutStr (IOStream h) = vPutStr h
    vFlush (IOStream h) = vFlush h
    vGetLine (IOStream h) = vGetLine h
    vIsEOF (IOStream h) = vIsEOF h
