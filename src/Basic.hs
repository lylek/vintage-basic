-- Basic.hs
-- The shell of the interpreter.  Processes command-line parameters,
-- calls the tokenizer, parser, prettyprinter and interpreter.
-- Lyle Kopnicky
-- last updated 2005-07-09

import Data.List
import System.Environment
import System.IO
import BasicSyntax
import Text.ParserCombinators.Parsec
import DurableTraps
import BasicMonad
import BasicTokenizer
import BasicParser
import BasicPrinter
import BasicInterp

main = do args <- getArgs
          sequence_ (intersperse divider [execute fn | fn <- args])

divider = do putStrLn ""
             putStrLn "------------------------------"
             putStrLn ""

execute :: FilePath -> IO ()
execute fn = do text <- readFile fn
                putStrLn ("Filename: " ++ fn)
                putStrLn "Source:"
                putStr text
		let [(tokens,"")] = tokensP $$ text
                case linesP $$ tokens
                     of [] -> error "!NO PARSE"
                        [(lineList,[])] -> executeLL lineList
                        [(lineList,extra)] -> incompleteParse lineList extra
                        _ -> error "!AMBIGUOUS PARSE"

incompleteParse lineList extra =
    error ("!INCOMPLETE PARSE\nparsed =\n"
           ++ printLines lineList)
--           ++ "unparsed =\n"
--           ++ extra)

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

lineOrdering :: Line -> Line -> Ordering
lineOrdering l1@(Line n1 sts1) l2@(Line n2 sts2) = compare n1 n2
lineOrdering l1@(Line n1 sts1) l2@(SyntaxError n2) = compare n1 n2
lineOrdering l1@(SyntaxError n1) l2@(Line n2 sts2) = compare n1 n2
lineOrdering l1@(SyntaxError n1) l2@(SyntaxError n2) = compare n1 n2

linesEq :: Line -> Line -> Bool
linesEq l1 l2 = lineOrdering l1 l2 == EQ

-- This function reverses before nubbing so that later lines take precedence.
sortNubLines :: [Line] -> [Line]
sortNubLines lineList =
    let sortedLines = sortBy lineOrdering lineList
        reversedSortedLines = reverse sortedLines
        reversedNubbedLines = nubBy linesEq reversedSortedLines
        in reverse reversedNubbedLines

-- This 'program' function interprets the list of lines.
-- Note that jumpTable and interpLine are mutually recursive.
-- The jumpTable contains interpreted code, which in turn calls
-- the jumpTable to look up code.  Since the jumpTable is a single
-- data structure, it memoizes interpreted code, making 'program'
-- a just-in-time compiler.  (The only time code is reinterpreted
-- is following an IF statement.)
program :: [Line] -> Code
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
