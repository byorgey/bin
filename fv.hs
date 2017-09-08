import           Control.Applicative
import           Control.Monad       (forM_, when)
import           Data.Char           (toLower)
import           Data.List           (isPrefixOf, sortBy)
import           Data.Maybe          (listToMaybe)
import           Data.Ord            (comparing)
import           System.Environment  (getArgs)
import           System.IO
import           System.Process

data Task = Task { taskNum :: Int, text :: String, marked :: Bool }
  deriving Show

mkTask n s = Task n s' mark
  where
    (mark, s') = case words s of
      ("(M)":rest) -> (True, unwords rest)
      _            -> (False, s)

parseMarkedTask = parseTask' . words
  where
    parseTask' (n:txt) = Task (read n) (unwords txt) True

mark (Task n _ _) = callCommand $ "todo.sh pri " ++ show n ++ " M"

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
      putStr $ "  " ++ niceText selTask ++ " ? ([y]es/[n]o/[d]elete/[f]inish/[a]bort) "
      hFlush stdout

      ans <- map toLower <$> getLine
      selTask' <- case ans of
        "y" -> do mark nextTask
                  return nextTask
        "d" -> do putStrLn "Deleting..."
                  callCommand $ "todo.sh -f -N del " ++ show (taskNum nextTask)
                  return selTask
        "f" -> do completeTask ["-a"] nextTask
                  return selTask
        _   -> return selTask

      case ans of
        "a" -> do putStrLn "Aborting task marking..."
                  unmarkAll
        _   -> markTasks' selTask' tasks

unmarkAll :: IO ()
unmarkAll = do
  marked <- readMarkedTasks
  forM_ marked depri

readMarkedTasks :: IO [Task]
readMarkedTasks =
  (reverse . sortBy (comparing taskNum)
   . map parseMarkedTask
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

getCaseNums :: Task -> [String]
getCaseNums t = map tail (filter ("#" `isPrefixOf`) (words (text t)))

oneTask :: Task -> IO Bool
oneTask t = do
  let caseNums = getCaseNums t
      prompt = "  " ++ concat ["(c)ase, " | not (null caseNums)]
                    ++ "(d)one, (r)eadd, (s)top, (q)uit? "
  putStr prompt
  hFlush stdout

  ans <- getLine
  case ans of
    "c" -> mapM_ visitCase caseNums >> oneTask t
    "d" -> do
      completeTask [] t
      return True
    "r" -> do
      depri t
      callCommand $ "todo.sh -f -n del " ++ show (taskNum t)
      callCommand $ "todo.sh add '" ++ text t ++ "'"
      return True
    "s" -> return False
    "q" -> unmarkAll >> return False
    _   -> oneTask t

completeTask :: [String] -> Task -> IO ()
completeTask args t = do
  putStrLn "Marking complete..."
  mapM_ closeCase (getCaseNums t)
  callCommand $ "todo.sh " ++ unwords args ++ " do " ++ show (taskNum t)

visitCase n = callCommand $ "firefox https://byorgey.fogbugz.com/f/cases/" ++ show n

closeCase n = do
  token <- (head . lines) <$> readFile "/home/brent/local/secret/fogbugz-token"
  callCommand $ curl ++ "'https://byorgey.fogbugz.com/api.asp?cmd=resolve&ixBug=" ++ n ++ "&token=" ++ token ++ "' >/dev/null"
  callCommand $ curl ++ "'https://byorgey.fogbugz.com/api.asp?cmd=close&ixBug=" ++ n ++ "&token=" ++ token ++ "' >/dev/null"
  where
    curl = "curl -s -H \"Accept: application/xml\" -H \"Content-Type: application/xml\" -X GET "

-- TODO: parse prioritization indicators, then auto-detect whether to continue or mark
-- based on presence of any existing marks.
main = do
  todos <- readFile "/home/brent/notes/todo/todo.txt"
  let tasks = zipWith mkTask [1..] (lines todos)

  case any marked tasks of
    True -> do
      putStrLn "Continuing..."
      doMarkedTasks
    False -> do
      markTasks tasks
      doMarkedTasks
