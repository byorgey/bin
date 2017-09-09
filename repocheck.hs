import Control.Monad
import System.Directory
import System.FilePath
import System.Process

checkRepo :: FilePath -> FilePath -> IO ()
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
    putStrLn $ (if dirty then "! " else "  ") ++ (if ahead > 0 then (show ahead) else " ") ++ " " ++ repo

main :: IO ()
main = do
  home <- getHomeDirectory
  repos <- lines <$> readFile (home </> ".repos")
  forM_ repos (checkRepo home)
