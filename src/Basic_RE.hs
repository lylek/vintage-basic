-- Basic.hs
-- The shell of the interpreter.  Processes command-line parameters,
-- calls the parser, prettyprinter and interpreter.
-- Lyle Kopnicky
-- last updated 2004-09-01

import List
import System
import BasicSyntax
import Parser
import BasicRT
import ResumableExceptions
import BasicMonad
import BasicParser
import BasicPrinter
import BasicInterp

main = do args <- getArgs
	  sequence_ (intersperse divider [execute fn | fn <- args])

divider = do putStrLn ""
	     putStrLn "------------------------------"
	     putStrLn ""

test1 = do text <- readFile "test1.bas"
	   let [(lineList,"")] = linesP `ap` text
	   putStr (printLines lineList)

execute :: FilePath -> IO ()
execute fn = do text <- readFile fn
		putStrLn ("Filename: " ++ fn)
		putStrLn "Source:"
		putStr text
		case linesP `ap` text
		     of [] -> error "!NO PARSE"
			[(lineList,"")] -> executeLL lineList
			[(lineList,extra)] -> incompleteParse lineList extra
			_ -> error "!AMBIGUOUS PARSE"

incompleteParse lineList extra =
    error ("!INCOMPLETE PARSE\nparsed =\n"
	   ++ printLines lineList 
	   ++ "unparsed =\n"
	   ++ extra)

executeLL :: [Line] -> IO ()
executeLL lineList =
    do putStrLn "\nPrettyprinted:"
       putStr (printLines lineList)
       if length [() | SyntaxError n <- lineList] > 0
	  then return ()
	  else do putStrLn "\nRun:"
		  let (sts,jt) = (program emptyJT lineList)
		      sts' = statements lineList
		      in do (Excep res _) <- runBasic (interp sts) jt
			    putStrLn "\nResult:"
			    print res

-- unJust (Just x) = x
-- putStr (unlines (map printStatement (unJust (BasicMonad.lookup tab 20))))

lineOrdering l1@(Line n1 sts1) l2@(Line n2 sts2) = compare n1 n2

statements :: [Line] -> [Statement]
statements lines = concat [sts | Line n sts <- lines]

-- Sorts lines and enters them into the jump table.  Returns a pair of
-- the whole statement list and the jump table.
-- Should remove duplicate lines, giving precedence to later ones; now
-- precedence is going to earlier ones.
program :: JumpTable -> [Line] -> ([Statement], JumpTable)
program jt ls =
    let sLines = sortBy lineOrdering ls
	(sts,jt') = foldr enterSt ([], jt) sLines
	in (sts, jt')
    where enterSt (Line n sts) (accumSts,jt) =
	      let accumSts' = sts++accumSts in
			      (accumSts', enterJT jt n accumSts')
