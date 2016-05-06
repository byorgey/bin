import           Control.Monad.Random
import           Data.Char            (toUpper)
import           Data.List            (isPrefixOf)
import           Data.Time
import           System.IO

weekday :: Day -> String
weekday = formatTime defaultTimeLocale "%A"

randomDay :: Rand StdGen Day
randomDay = ModifiedJulianDay <$> getRandomR (start, end)
  where
    start = toModifiedJulianDay (fromGregorian 1700 1 1)
    end   = toModifiedJulianDay (fromGregorian 2299 12 31)

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%B %-d, %Y"

main :: IO ()
main = do
  d <- evalRandIO randomDay
  putStr (showDay d ++ "? ")
  hFlush stdout
  start <- getCurrentTime
  guess <- getLine
  stop  <- getCurrentTime
  putStrLn $ if (map toUpper guess `isPrefixOf` map toUpper (weekday d)) then "Right!" else ("Nope, " ++ weekday d ++ ".")
  putStrLn $ "Time: " ++ show (diffUTCTime stop start)

