import           Control.Applicative
import           Control.Monad       (forM_, when)
import           Data.Char           (toLower)
import           Data.List           (isPrefixOf, sortBy)
import           Data.Maybe          (listToMaybe)
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
  callCommand "todo.sh archive"
  where
    markTasks' :: Task -> [Task] -> IO ()
    markTasks' selTask [] = return ()
    markTasks' selTask (nextTask:tasks) = do
      putStrLn "\nWould you like to"
      putStrLn $ "  " ++ niceText nextTask
      putStrLn "before"
      putStr $ "  " ++ niceText selTask ++ " ? ([y]es/[n]o/[d]elete/[f]inish) "
      hFlush stdout

      ans <- map toLower <$> getLine
      selTask' <- case ans of
        "y" -> do mark nextTask
                  return nextTask
        "d" -> do putStrLn "Deleting..."
                  callCommand $ "todo.sh -f -N del " ++ show (taskNum nextTask)
                  return selTask
        "f" -> do putStrLn "Marking complete..."
                  callCommand $ "todo.sh -a do " ++ show (taskNum nextTask)
                  return selTask
        _   -> return selTask

      markTasks' selTask' tasks

unmarkAll :: IO ()
unmarkAll = do
  marked <- readMarkedTasks
  forM_ marked depri

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
      continue <- oneTask t
      when continue $ doMarkedTasks

depri :: Task -> IO ()
depri t = callCommand $ "todo.sh depri " ++ show (taskNum t)

oneTask :: Task -> IO Bool
oneTask t = do
  let caseNums = map tail (filter ("#" `isPrefixOf`) (words (text t)))
      prompt = "  " ++ concat ["(c)ase, " | not (null caseNums)]
                    ++ "(d)one, (r)eadd, (q)uit? "
  putStr prompt
  hFlush stdout

  ans <- getLine
  case ans of
    "c" -> mapM_ visitCase caseNums >> oneTask t
    "d" -> do
      mapM_ closeCase caseNums
      callCommand $ "todo.sh do " ++ show (taskNum t)
      return True
    "r" -> do
      depri t
      callCommand $ "todo.sh -f -n del " ++ show (taskNum t)
      callCommand $ "todo.sh add '" ++ text t ++ "'"
      return True
    "q" -> do
      unmarkAll
      return False
    _   -> oneTask t

visitCase n = callCommand $ "firefox https://byorgey.fogbugz.com/f/cases/" ++ show n

closeCase n = do
  token <- (head . lines) <$> readFile "/home/brent/local/secret/fogbugz-token"
  callCommand $ curl ++ "'https://byorgey.fogbugz.com/api.asp?cmd=resolve&ixBug=" ++ n ++ "&token=" ++ token ++ "' >/dev/null"
  callCommand $ curl ++ "'https://byorgey.fogbugz.com/api.asp?cmd=close&ixBug=" ++ n ++ "&token=" ++ token ++ "' >/dev/null"
  where
    curl = "curl -s -H \"Accept: application/xml\" -H \"Content-Type: application/xml\" -X GET "

main = do
  todos <- readFile "/home/brent/notes/todo/todo.txt"
  let tasks = zipWith mkTask [1..] (lines todos)

  markTasks tasks
  doMarkedTasks
