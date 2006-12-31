-- Basic.hs
-- The shell of the interpreter.  Processes command-line parameters,
-- calls the tokenizer, parser, prettyprinter and interpreter.
-- Lyle Kopnicky

import Data.List(deleteFirstsBy,mapAccumR,nubBy,sortBy)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.IO(hFlush,stdout)
import BasicSyntax(Line(..))
import Text.ParserCombinators.Parsec(parse,getPosition,setPosition,setSourceColumn,setSourceLine)
import DurableTraps(Excep(..),done)
import BasicMonad -- just a hack - remove this later
import BasicLexCommon(Tagged)
import BasicLineScanner(RawLine(..),rawLinesP)
import BasicTokenizer(Token,TokenizedLine(..),taggedTokensP)
import BasicParser(statementListP)
import BasicPrinter(printLines)
import BasicInterp(interpLines)

-- TODO: Consider sending errors to stderr.
-- TODO: On syntax error, consider printing line with marked error.

main = do args <- getArgs
          sequence_ [execute fileName | fileName <- args]

execute :: FilePath -> IO ()
execute fileName =
    do text <- readFile fileName
       rawLines <- scanLines fileName text
       tokenizedLines <- sequence [tokenizeLine fileName rawLine | rawLine <- rawLines]
       parsedLines <- sequence [parseLine tokenizedLine | tokenizedLine <- tokenizedLines]
       hFlush stdout
       (Excep r _ _, BasicState lineNum _) <- runBasic $ interpLines parsedLines
       putStrLn "\nResult:"
       putStr (show r ++ " IN LINE " ++ show lineNum)

scanLines :: String -> String -> IO [RawLine]
scanLines fileName text =
    case parse rawLinesP fileName text
         of (Left parseError) -> do putStrLn ("!SYNTAX ERROR IN RAW "
                                              ++ show parseError)
                                    exitFailure
            (Right rawLines) -> sortNubLines rawLines

tokenizeLine :: String -> RawLine -> IO TokenizedLine
tokenizeLine fileName (RawLine lineNum colNum text) =
    let setPositionAndTokenize =
	    do pos <- getPosition
	       setPosition $ setSourceColumn (setSourceLine pos lineNum) colNum
	       taggedTokensP
	in case parse setPositionAndTokenize fileName text
           of (Left parseError) -> do putStrLn ("!SYNTAX ERROR " ++ show parseError)
                                      exitFailure
              (Right taggedTokens) -> return (TokenizedLine lineNum taggedTokens)

parseLine :: TokenizedLine -> IO Line
parseLine (TokenizedLine lineNum taggedTokens) =
    case parse statementListP "" taggedTokens
         of (Left parseError) -> do putStrLn ("!SYNTAX ERROR " ++ show parseError)
                                    exitFailure
            (Right statementList) -> return (Line lineNum statementList)

rawLineOrdering :: RawLine -> RawLine -> Ordering
rawLineOrdering (RawLine n1 _ _) (RawLine n2 _ _) = compare n1 n2

rawLinesEq :: RawLine -> RawLine -> Bool
rawLinesEq l1 l2 = rawLineOrdering l1 l2 == EQ

-- This function reverses before nubbing so that later lines take precedence.
sortNubLines :: [RawLine] -> IO [RawLine]
sortNubLines lineList =
    do let sortedLines = sortBy rawLineOrdering lineList
           reversedSortedLines = reverse sortedLines
	   reversedNubbedLines = nubBy rawLinesEq reversedSortedLines
	   nubbedLines = reverse reversedNubbedLines
	   duplicateLines = deleteFirstsBy rawLinesEq lineList nubbedLines
       sequence_ [putStrLn ("!SUPERSEDING PREVIOUS LINE " ++ show n)
		  | (RawLine n _ _) <- duplicateLines]
       return nubbedLines

-- Other things we could do:
-- * Pre-check types
-- * Pre-check labels, generate code in place of labels
-- * Convert variable references to IORefs
-- Is it easiest to do these with staging?
