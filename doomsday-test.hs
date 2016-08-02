import           Control.Monad.Random
import           Data.Char            (toUpper)
import           Data.List            (isPrefixOf)
import           Data.Time

import           Doomsday

weekday :: Day -> String
weekday = formatTime defaultTimeLocale "%A"

weekdayNum :: Day -> String
weekdayNum = formatTime defaultTimeLocale "%w"

randomDay :: Rand StdGen Day
randomDay = ModifiedJulianDay <$> getRandomR (start, end)
  where
    start = toModifiedJulianDay (fromGregorian 1700 1 1)
    end   = toModifiedJulianDay (fromGregorian 2299 12 31)

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%B %-d, %Y"

doomsdayTest :: IO (Bool, NominalDiffTime)
doomsdayTest = runTest randomDay showDay
  (\guess d ->
     ( let g = map toUpper guess in (length g > 1 && g `isPrefixOf` map toUpper (weekday d)) || (guess == weekdayNum d)
     , weekday d
     )
  )

main :: IO ()
main = runTests doomsdayTest
