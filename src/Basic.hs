import System.Environment(getArgs)
import System.IO
import BasicExecuter(executeFile)
import BasicMonad(runProgram)
import DurableTraps(done)
import IOStream

main :: IO ()
main = do
    args <- getArgs
    runProgram (IOStream stdin) (IOStream stdout) $ do
        sequence_ [executeFile fileName | fileName <- args]
        done
