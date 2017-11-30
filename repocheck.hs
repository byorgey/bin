import           Control.Monad
import           System.Directory
import           System.FilePath
import           System.Process

-- TODO:
--   1. autodetect git vs darcs
--   2. colorize output?
--   3. inline, interactive rectification (i.e. opportunity to record
--   and push in each repo before moving on to the next)?
--   hmm... seems like more trouble than it's worth.  Just open a new
--   terminal.

checkRepo :: FilePath -> FilePath -> IO Bool   -- True <=> clean
checkRepo home repo = do
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
