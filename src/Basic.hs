-- Basic.hs
-- The shell of the interpreter.  Processes command-line parameters,
-- calls the tokenizer, parser, prettyprinter and interpreter.
-- Lyle Kopnicky
-- last updated 2005-07-09

import Data.List(sortBy,nubBy,deleteFirstsBy)
import System.Environment(getArgs)
--import System.IO
--import BasicSyntax
import Text.ParserCombinators.Parsec(parse,getPosition,setPosition,setSourceColumn,setSourceLine)
--import DurableTraps
--import BasicMonad
import BasicLineScanner(RawLine(..),rawLinesP)
import BasicTokenizer(tokenize)
--import BasicParser
--import BasicPrinter
--import BasicInterp

-- TODO: Consider sending errors to stderr.
-- TODO: On syntax error, consider printing line with marked error.

main = do args <- getArgs
          sequence_ [execute fileName | fileName <- args]

execute :: FilePath -> IO ()
execute fileName =
    do text <- readFile fileName
       case parse rawLinesP fileName text
	    of (Left parseError) -> putStrLn ("!SYNTAX ERROR IN RAW "
					      ++ show parseError)
	       (Right rawLines) ->
		   do orderedLines <- sortNubLines rawLines
		      let tokenizedLines =
			      [tokenizeRawLine fileName l | l <- orderedLines]
		      print tokenizedLines

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

tokenizeRawLine fileName (RawLine lin col s) =
    let setPositionAndTokenize =
	    do pos <- getPosition
	       setPosition $ setSourceColumn (setSourceLine pos lin) col
	       tokenize
	in parse setPositionAndTokenize fileName s

{-
executeLL :: [Line] -> IO ()
executeLL lineList =
    do let snLines = sortNubLines lineList
       putStrLn "\nPrettyprinted:"
       putStr (printLines snLines)
       if length [() | SyntaxError n <- lineList] > 0
          then return ()
          else do putStrLn "\nRun:"
                  hFlush stdout
                  (Excep r _ _) <- runBasic $ program snLines
                  putStrLn "\nResult:"
                  print r

-- This 'program' function interprets the list of lines.
-- Note that jumpTable and interpLine are mutually recursive.
-- The jumpTable contains interpreted code, which in turn calls
-- the jumpTable to look up code.  Since the jumpTable is a single
-- data structure, it memoizes interpreted code, making 'program'
-- a just-in-time compiler.  (The only time code is reinterpreted
-- is following an IF statement.)
program :: [Line] -> Program
program lines =
    let interpLine (Line lab stmts) =
            (lab, mapM_ (interpS jumpTable) stmts)
        makeTableEntry accumCode (lab, codeSeg) =
            let accumCode' = codeSeg >> accumCode
                in (accumCode', (lab, accumCode'))
        jumpTable = snd $ mapAccumR makeTableEntry done $ map interpLine lines
        in snd $ head jumpTable

-- Other things we could do:
-- * Pre-check types
-- * Pre-check labels, generate code in place of labels
-- * Convert variable references to IORefs
-- Is it easiest to do these with staging?

-}