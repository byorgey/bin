import           Control.Applicative
import           System.IO

data Task = Task { marked :: Bool, text :: String }
  deriving Show

mkTask = Task False

mark (Task _ t) = Task True t

niceText = nicify . text
  where
    nicify = unwords . filter (\w -> not (head w `elem` "@#+")) . words

main = do
  todos <- readFile "/home/brent/notes/todo/todo.txt"
  let tasks = map mkTask (lines todos)

  markedTasks <- case tasks of
                   (task1:rest) -> (mark task1 :) <$> markTasks task1 rest
                   []           -> return []

markTasks :: Task -> [Task] -> IO [Task]
markTasks selTask [] = return []
markTasks selTask (nextTask:tasks) = do
  putStrLn "\nWould you like to"
  putStrLn $ "  " ++ niceText nextTask
  putStrLn "before"
  putStr $ "  " ++ niceText selTask ++ " ?  "
  hFlush stdout

  ans <- getLine
  if ans == "y" || ans == "Y"
    then (mark nextTask :) <$> markTasks nextTask tasks
    else markTasks selTask tasks
