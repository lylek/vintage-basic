-- BasicExecuter.hs
-- The shell of the interpreter.  Processes command-line parameters,
-- calls the tokenizer, parser, prettyprinter and interpreter.
-- Lyle Kopnicky

module BasicExecuter where

import Control.Monad.State(get)
import Control.Monad.Trans(liftIO)
import Data.List(deleteFirstsBy,nubBy,sortBy)
import Text.ParserCombinators.Parsec(parse,setPosition,sourceLine)
import BasicInterp(interpLines)
import BasicLexCommon(Tagged(..))
import BasicLineScanner(RawLine,rawLinesP)
import BasicMonad(BasicState(..),Code,printString,runProgram)
import BasicParser(statementListP)
import BasicResult(BasicResult(..))
import BasicSyntax(Line(..))
import BasicTokenizer(TokenizedLine,taggedTokensP)
import DurableTraps(die)

executeFile :: FilePath -> Code ()
executeFile fileName = do
    text <- liftIO $ readFile fileName
    execute fileName text

execute :: FilePath -> String -> Code ()
execute fileName text = do
    rawLines <- scanLines fileName text
    tokenizedLines <- sequence [tokenizeLine rawLine | rawLine <- rawLines]
    parsedLines <- sequence [parseLine tokenizedLine | tokenizedLine <- tokenizedLines]
    state <- get
    liftIO $ runProgram (inputStream state) (outputStream state) $ interpLines parsedLines

scanLines :: String -> String -> Code [RawLine]
scanLines fileName text =
    case parse rawLinesP fileName text of
        (Left parseError) -> die (ScanError parseError)
        (Right rawLines)  -> sortNubLines rawLines

tokenizeLine :: RawLine -> Code TokenizedLine
tokenizeLine (Tagged pos text) =
    case parse (setPosition pos >> taggedTokensP) "" text of
        (Left parseError)    -> die (SyntaxError parseError)
        (Right taggedTokens) -> return (Tagged pos taggedTokens)

parseLine :: TokenizedLine -> Code Line
parseLine (Tagged pos taggedTokens) = do
    case parse (setPosition pos >> statementListP) "" taggedTokens of
        (Left parseError)     -> die (SyntaxError parseError)
        (Right statementList) -> return (Line (sourceLine pos) statementList)

rawLineOrdering :: RawLine -> RawLine -> Ordering
rawLineOrdering (Tagged pos1 _) (Tagged pos2 _) = compare (sourceLine pos1) (sourceLine pos2)

rawLinesEq :: RawLine -> RawLine -> Bool
rawLinesEq l1 l2 = rawLineOrdering l1 l2 == EQ

-- | This function reverses before nubbing so that later lines take precedence.
sortNubLines :: [RawLine] -> Code [RawLine]
sortNubLines lineList = do
    let sortedLines = sortBy rawLineOrdering lineList
        reversedSortedLines = reverse sortedLines
        reversedNubbedLines = nubBy rawLinesEq reversedSortedLines
        nubbedLines = reverse reversedNubbedLines
        duplicateLines = deleteFirstsBy rawLinesEq lineList nubbedLines
    sequence_ [printString ("!SUPERSEDING PREVIOUS LINE " ++ show (sourceLine pos) ++ "\n")
        | (Tagged pos _) <- duplicateLines]
    return nubbedLines
