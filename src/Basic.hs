import System.Environment(getArgs)
import BasicExecuter(execute)

main = do
    args <- getArgs
    sequence_ [execute fileName | fileName <- args]
