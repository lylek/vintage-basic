import System.Environment(getArgs,getProgName)
import System.Exit
import System.IO
import Control.Monad(unless)
import Control.Monad.CPST.DurableTraps(done)
import IO.IOStream
import Language.VintageBasic.Executer(executeFile)
import Language.VintageBasic.BasicMonad(runProgram)

-- | Runs a BASIC program specified on the command line.
main :: IO ()
main = do
    args <- getArgs
    unless (length args == 1) $ do
        progName <- getProgName
        hPutStrLn stderr ("Usage: " ++ progName ++ " SOURCE_FILE.bas")
        exitFailure
    runProgram (IOStream stdin) (IOStream stdout) $ do
        sequence_ [executeFile fileName | fileName <- args]
        done
