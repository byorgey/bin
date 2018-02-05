-- Compile to 'rc'

import           Data.List        (isPrefixOf)

import           Control.Monad
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process

-- TODO:
--   2. colorize output?
--   3. inline, interactive rectification (i.e. opportunity to record
--   and push in each repo before moving on to the next)?
--   hmm... seems like more trouble than it's worth.  Just open a new
--   terminal.

checkGitRepo :: FilePath -> FilePath -> IO (Bool, Int)
  -- True <=> dirty, # ahead
checkGitRepo home repo = do
  out <- readCreateProcess
           (shell "git status --porcelain 2> /dev/null | tail -n1")
             { cwd = Just (home </> repo) }
           ""
  let dirty = out /= ""

  out <- readCreateProcess
           (shell "git rev-list @{upstream}..HEAD 2>/dev/null | wc -l")
             { cwd = Just (home </> repo) }
           ""

  let ahead = read out :: Int

  return (dirty, ahead)

checkDarcsRepo :: FilePath -> FilePath -> IO (Bool, Int)
checkDarcsRepo home repo = do
  -- putStrLn $ "Checking darcs repo: " ++ repo
  (exit, out, _) <- readCreateProcessWithExitCode
                      (shell "darcs whatsnew -s --look-for-adds")
                        { cwd = Just (home </> repo) }
                      ""
  let dirty = case exit of
        ExitFailure _ -> False
        ExitSuccess   -> True

  out <- readCreateProcess
           (shell "darcs push --dry-run")
             { cwd = Just (home </> repo) }
           ""

  let ahead = length . filter ("patch" `isPrefixOf`) . lines $ out

  return (dirty, ahead)

checkRepo :: FilePath -> FilePath -> IO Bool   -- True <=> clean
checkRepo home repo = do
  isGit <- (".git" `elem`) <$> listDirectory (home </> repo)
  (dirty, ahead) <- case isGit of
    True  -> checkGitRepo home repo
    False -> checkDarcsRepo home repo

  when (dirty || ahead > 0) $ do
    putStrLn $ (if dirty then "! " else "  ") ++
               (if ahead > 0 then (show ahead) else " ") ++ " " ++ repo

  return (not dirty && ahead == 0)

main :: IO ()
main = do
  home <- getHomeDirectory
  repos <- lines <$> readFile (home </> ".repos")
  clean <- and <$> forM repos (checkRepo home)
  when clean $ putStrLn "All clean!"
