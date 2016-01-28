import           Control.Monad.Random
import           Data.Time
import           System.IO
import           System.Random

weekday :: Day -> String
weekday = formatTime defaultTimeLocale "%A"

randomDay :: Rand StdGen Day
randomDay = ModifiedJulianDay <$> getRandomR (start, end)
  where
    start = toModifiedJulianDay (fromGregorian 1700 1 1)
    end   = toModifiedJulianDay (fromGregorian 2299 12 31)

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%B %-d, %Y"

main = do
  d <- evalRandIO randomDay
  putStr (showDay d ++ "? ")
  hFlush stdout
  guess <- getLine
  putStrLn $ if (guess == weekday d) then "Right!" else ("Nope, " ++ weekday d ++ ".")

