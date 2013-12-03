{-# LANGUAGE ViewPatterns #-}

import System.IO
import System.Directory
import System.Process
import System.FilePath
import Control.Applicative
import Control.Monad

import Data.Maybe

mailboxesrc = ".mailboxesrc"
maildir = "Maildir"

data Box = Box { boxName :: String, boxLoc :: FilePath, new :: Int }

readBox :: String -> Box
readBox (words -> [name, loc]) = Box name loc 0

main = do
  hSetBuffering stdout NoBuffering
  home  <- getHomeDirectory
  boxes <- map readBox . lines <$> readFile (home </> mailboxesrc)
  mainloop home boxes

nonEmpty :: Box -> Bool
nonEmpty = (>0) . new

mainloop :: FilePath -> [Box] -> IO ()
mainloop home boxes = do
  boxes' <- filter nonEmpty <$> mapM (checkBox (home </> maildir)) boxes
  putStr $ showCounts boxes'
  putStr "> "
  choice <- readLnMaybe
  case choice of
    Nothing -> mainloop home boxes'
    Just 0 -> return ()
    Just i -> do if (i <= length boxes')
                   then openBox (boxes' !! (i - 1))
                   else mapM_ openBox (reverse boxes')
                 mainloop home boxes'

openBox :: Box -> IO ()
openBox box = do
  system $ "m " ++ boxName box
  return ()

readLnMaybe :: (Read a) => IO (Maybe a)
readLnMaybe = readMaybe <$> getLine

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(x,[])] -> Just x
                _        -> Nothing

checkBox :: FilePath -> Box -> IO Box
checkBox dir box = do
  n <- countNew dir box
  return $ box { new = n }

countNew :: FilePath -> Box -> IO Int
countNew dir box = countEm `catch` const (return 0)
  where countEm = (subtract 2) . length <$> getDirectoryContents (dir </> boxLoc box </> "new")

showCounts :: [Box] -> String
showCounts = unlines . zipWith number [1..] . map showCount
  where
    number n s = show n ++ ") " ++ s
    showCount (Box name _ n) = name ++ ": " ++ show n
