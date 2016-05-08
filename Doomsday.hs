module Doomsday where

import           Control.Monad
import           Control.Monad.Random
import           Data.Time
import           System.Environment
import           System.IO

runTest :: Rand StdGen a -> (a -> String) -> (String -> a -> (Bool, String))
        -> IO (Bool, NominalDiffTime)
runTest pick display validate = do
  val <- evalRandIO pick
  putStr $ display val ++ "? "
  hFlush stdout
  start <- getCurrentTime
  guess <- getLine
  stop <- getCurrentTime
  let (correct, answer) = validate guess val
  when (not correct) $ putStrLn ("Wrong, " ++ answer)
  return (correct, diffUTCTime stop start)

runTests :: IO (Bool, NominalDiffTime) -> IO ()
runTests test = do
  argv <- getArgs
  let n = case argv of
            []     -> (1 :: Int)
            (ns:_) -> read ns
  results <- replicateM n test
  when (n > 1) $ putStrLn $ show (length (filter fst results)) ++ " correct out of " ++ show n
  let elapsed = sum (map snd results)
  putStrLn $ "Total time: " ++ show elapsed
  when (n > 1) $ do
    putStrLn $ "Best      : " ++ show (minimum $ map snd results)
    putStrLn $ "Worst     : " ++ show (maximum $ map snd results)
    putStrLn $ "Average   : " ++ show (elapsed / fromIntegral n)

