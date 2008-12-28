-- | The shell of the interpreter.  It processes command-line parameters,
-- and calls the line scanner, tokenizer, parser, and interpreter.

module Language.VintageBasic.Executer where

import Control.Monad.CPST.DurableTraps(die)
import Control.Monad.State(get)
import Control.Monad.Trans(liftIO)
import Data.List(deleteFirstsBy,nubBy,sortBy)
import Language.VintageBasic.Interpreter(interpLines)
import Language.VintageBasic.LexCommon(Tagged(..))
import Language.VintageBasic.LineScanner(RawLine,rawLinesP)
import Language.VintageBasic.BasicMonad(BasicState(..),Code,printString,runProgram)
import Language.VintageBasic.Parser(statementListP)
import Language.VintageBasic.Result(Result(..))
import Language.VintageBasic.Syntax(Line(..))
import Language.VintageBasic.Tokenizer(TokenizedLine,taggedTokensP)
import Text.ParserCombinators.Parsec(parse,setPosition,sourceLine)

-- | Given a file path, loads and executes the BASIC code.
executeFile :: FilePath -> Code ()
executeFile fileName = do
    text <- liftIO $ readFile fileName
    execute fileName text

-- | Executes BASIC code from a string. The file path is provided only for error reporting.
execute :: FilePath -> String -> Code ()
execute fileName text = do
    rawLines <- scanLines fileName text
    tokenizedLines <- sequence [tokenizeLine rawLine | rawLine <- rawLines]
    parsedLines <- sequence [parseLine tokenizedLine | tokenizedLine <- tokenizedLines]
    state <- get
    liftIO $ runProgram (inputStream state) (outputStream state) $ interpLines parsedLines

-- | Transforms the BASIC source into a series of 'RawLine's using the 'rawLinesP' LineScanner.
scanLines :: String -> String -> Code [RawLine]
scanLines fileName text =
    case parse rawLinesP fileName text of
        (Left parseError) -> die (ScanError parseError)
        (Right rawLines)  -> sortNubLines rawLines

-- | Tokenizes a 'RawLine' into a 'TokenizedLine' using the 'taggedTokensP' Tokenizer.
tokenizeLine :: RawLine -> Code TokenizedLine
tokenizeLine (Tagged pos text) =
    case parse (setPosition pos >> taggedTokensP) "" text of
        (Left parseError)    -> die (SyntaxError parseError)
        (Right taggedTokens) -> return (Tagged pos taggedTokens)

-- | Parses a 'TokenizedLine' to yield a 'Line', using the 'statementListP' Parser.
parseLine :: TokenizedLine -> Code Line
parseLine (Tagged pos taggedTokens) = do
    case parse (setPosition pos >> statementListP) "" taggedTokens of
        (Left parseError)     -> die (SyntaxError parseError)
        (Right statementList) -> return (Line (sourceLine pos) statementList)

-- | Specifies an ordering for 'RawLine's so they can be sorted.
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
