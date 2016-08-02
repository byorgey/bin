import           Control.Applicative
import           Control.Monad       (forM_)
import           Data.List           (sortBy)
import           Data.Ord            (comparing)
import           System.IO
import           System.Process

data Task = Task { taskNum :: Int, text :: String }
  deriving Show

mkTask = Task

parseTask = parseTask' . words
  where
    parseTask' (n:txt) = Task (read n) (unwords txt)

mark (Task n _) = callCommand $ "todo.sh pri " ++ show n ++ " M"

niceText = nicify . text
  where
    nicify = unwords . filter (\w -> not (head w `elem` "@#+")) . words

markTasks :: [Task] -> IO ()
markTasks [] = return ()
markTasks (task1:rest) = do
  mark task1
  markTasks' task1 rest
  where
    markTasks' :: Task -> [Task] -> IO ()
    markTasks' selTask [] = return ()
    markTasks' selTask (nextTask:tasks) = do
      putStrLn "\nWould you like to"
      putStrLn $ "  " ++ niceText nextTask
      putStrLn "before"
      putStr $ "  " ++ niceText selTask ++ " ?  "
      hFlush stdout

      ans <- getLine
      if ans == "y" || ans == "Y"
        then mark nextTask >> markTasks' nextTask tasks
        else markTasks' selTask tasks

readMarkedTasks :: IO [Task]
readMarkedTasks =
  (reverse . sortBy (comparing taskNum)
   . map parseTask
   . reverse . drop 2 . reverse
   . lines)
  <$> readProcess "todo.sh" ["-p", "-P", "listpri", "M"] ""

doMarkedTasks :: IO ()
doMarkedTasks = do
  taskList <- readMarkedTasks
  case taskList of
    []     -> return ()
    (t:ts) -> do
      putStrLn $ "\n*** Next: " ++ niceText t
      doneOrReadd t
      doMarkedTasks

doneOrReadd t = do
  putStr $ "  (d)one, (r)eadd? "
  hFlush stdout

  ans <- getLine
  case ans of
    "d" -> callCommand $ "todo.sh do " ++ show (taskNum t)
    "r" -> do
      callCommand $ "todo.sh depri " ++ show (taskNum t)
      callCommand $ "todo.sh -f -n del " ++ show (taskNum t)
      callCommand $ "todo.sh add '" ++ text t ++ "'"
    _   -> doneOrReadd t

main = do
  todos <- readFile "/home/brent/notes/todo/todo.txt"
  let tasks = zipWith mkTask [1..] (lines todos)

  markTasks tasks
  doMarkedTasks
