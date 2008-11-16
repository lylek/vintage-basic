-- BasicExecuter.hs
-- The shell of the interpreter.  Processes command-line parameters,
-- calls the tokenizer, parser, prettyprinter and interpreter.
-- Lyle Kopnicky

module BasicExecuter where

import Data.List(deleteFirstsBy,nubBy,sortBy)
import System.Exit(exitFailure)
import BasicSyntax(Line(..))
import Text.ParserCombinators.Parsec(parse,setPosition,sourceLine)
import Text.ParserCombinators.Parsec.Error(ParseError,errorPos,errorMessages,showErrorMessages)
import BasicMonad -- just a hack - remove this later
import BasicLexCommon(Tagged(..))
import BasicLineScanner(RawLine,rawLinesP)
import BasicParser(statementListP)
import BasicInterp(interpLines)
import BasicTokenizer(TokenizedLine,taggedTokensP)

-- TODO: Consider sending errors to stderr.
-- TODO: On syntax error, consider printing line with marked error.

execute :: FilePath -> IO ()
execute fileName = do
    text <- readFile fileName
    rawLines <- scanLines fileName text
    tokenizedLines <- sequence [tokenizeLine rawLine | rawLine <- rawLines]
    parsedLines <- sequence [parseLine tokenizedLine | tokenizedLine <- tokenizedLines]
    runProgram $ interpLines parsedLines

scanLines :: String -> String -> IO [RawLine]
scanLines fileName text =
    case parse rawLinesP fileName text of
        (Left parseError) -> do
            putStrLn $ showLineNumberingError parseError
            exitFailure
        (Right rawLines) -> sortNubLines rawLines

tokenizeLine :: RawLine -> IO TokenizedLine
tokenizeLine (Tagged pos text) =
    case parse (setPosition pos >> taggedTokensP) "" text of
        (Left parseError) -> do
            putStrLn ("!SYNTAX ERROR AT " ++ show parseError)
            exitFailure
        (Right taggedTokens) ->
            return (Tagged pos taggedTokens)

parseLine :: TokenizedLine -> IO Line
parseLine (Tagged pos taggedTokens) = do
    case parse (setPosition pos >> statementListP) "" taggedTokens of
        (Left parseError) -> do
            putStrLn $ showSyntaxError parseError
            exitFailure
        (Right statementList) ->
            return (Line (sourceLine pos) statementList)

rawLineOrdering :: RawLine -> RawLine -> Ordering
rawLineOrdering (Tagged pos1 _) (Tagged pos2 _) = compare (sourceLine pos1) (sourceLine pos2)

rawLinesEq :: RawLine -> RawLine -> Bool
rawLinesEq l1 l2 = rawLineOrdering l1 l2 == EQ

-- This function reverses before nubbing so that later lines take precedence.
sortNubLines :: [RawLine] -> IO [RawLine]
sortNubLines lineList = do
    let sortedLines = sortBy rawLineOrdering lineList
        reversedSortedLines = reverse sortedLines
        reversedNubbedLines = nubBy rawLinesEq reversedSortedLines
        nubbedLines = reverse reversedNubbedLines
        duplicateLines = deleteFirstsBy rawLinesEq lineList nubbedLines
    sequence_ [putStrLn ("!SUPERSEDING PREVIOUS LINE " ++ show (sourceLine pos))
        | (Tagged pos _) <- duplicateLines]
    return nubbedLines

showParseError :: String -> String -> String -> ParseError -> String
showParseError msgErrorType msgLine msgEndOfInput parseError =
    let pos = errorPos parseError
        messages = errorMessages parseError
        line = sourceLine pos
    in
        "!" ++ msgErrorType ++ " ERROR IN " ++ msgLine ++ " " ++ show line
        ++ showErrorMessages "OR" " UNKNOWN" " EXPECTING" " UNEXPECTED" msgEndOfInput messages

showSyntaxError :: ParseError -> String
showSyntaxError = showParseError "SYNTAX" "LINE" "END OF LINE"

showLineNumberingError :: ParseError -> String
showLineNumberingError = showParseError "LINE NUMBERING" "RAW LINE" "END OF FILE"
