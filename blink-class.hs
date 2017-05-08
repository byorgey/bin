{-# LANGUAGE GADTs #-}

module Blink where

import           Control.Arrow               (first)
import           Control.Concurrent          (threadDelay)
import           Control.Monad               (when)
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.SRGB
import           Data.List                   (find, intercalate, sortBy)
import           Data.Ord                    (comparing)
import           Data.Time
import           Data.Time.Calendar.WeekDate
import           System.Environment          (getArgs)
import           System.Process
import           Text.Printf

data BlinkCommand
  = Off
  | On (Colour Double)
  | Blink (Colour Double) Int
  | Quit
  deriving Show

showColor :: Colour Double -> String
showColor c = intercalate "," (map show [r,g,b])
  where
    RGB r g b = toSRGB24 c

runBlinkCommand :: BlinkCommand -> IO ()
runBlinkCommand Quit = runBlinkCommand Off
runBlinkCommand c    = do
    putStrLn (debugMsg c)
    putStrLn $ "blink1-tool " ++ intercalate " " (blinkArgs c)
    callProcess "/home/brent/local/bin/blink1-tool" (blinkArgs c)
  where
    blinkArgs Off           = ["--off"]
    blinkArgs (On clr)      = ["--rgb", showColor clr]
    blinkArgs (Blink clr n) = blinkArgs (On clr) ++ ["--blink=" ++ show n]
    debugMsg Off = "Turning off..."
    debugMsg (On clr) = "Changing to color " ++ show clr ++ "..."
    debugMsg (Blink clr n) = "Blinnking color " ++ show clr ++ " " ++ show n ++ " times..."

-- | Minutes since midnight
type Minutes = Int

-- | Day of the week, 0 = Sunday
type DOW = Int

-- Invariant: should be sorted by time
type BlinkIntervals = [(Minutes, BlinkCommand)]

blinkIntervals :: BlinkIntervals -> IO ()
blinkIntervals intervals = go Nothing
  where
    go current = do
      delay 1
      mins <- fst <$> currentTime
      let int = findInterval mins intervals
      case int of
        Nothing -> go current
        Just (time, cmd) -> do
          when (Just time /= current) $ runBlinkCommand cmd
          case cmd of
            Quit -> return ()
            _    -> go $ Just time

currentTime :: IO (Minutes, DOW)
currentTime = do
  loc <- zonedTimeToLocalTime <$> getZonedTime
  let tod  = localTimeOfDay loc
      mins = todHour tod * 60 + todMin tod
      dow  = (\(_,_,w) -> w `mod` 7) . toWeekDate . localDay $ loc
  return (mins, dow)

findInterval :: Minutes -> BlinkIntervals -> Maybe (Minutes, BlinkCommand)
findInterval mins = safeLast . takeWhile ((<= mins) . fst)
  where
    safeLast [] = Nothing
    safeLast xs = Just $ last xs

delay :: Int -> IO ()
delay secs = threadDelay (1000000 * secs)

twoMinutes :: BlinkIntervals
twoMinutes = [(784, On red), (785, On blue), (786, Quit)]

classCommands = [On orange, On cyan, On green, On blue, On purple, On yellow, On red, Blink red 100]

hrs :: Int -> Minutes
hrs h
  | h < 8     = 60 * (h + 12)
  | otherwise = 60 * h

trOffsets :: [Minutes]
trOffsets = [-200, -1, 0, 45, 60, 70, 73, 75]

mwfOffsets :: [Minutes]
mwfOffsets = [-200, -1, 0, 25, 35, 45, 48, 50]

time :: Int -> Int -> Minutes
time h m = hrs h + m

data Period = Period { pdOffsets :: [Minutes], pdDays :: [DOW], pdName :: String, pdStart :: Minutes }
  deriving Show

pdIntervals :: Period -> BlinkIntervals
pdIntervals p = zip (map (pdStart p +) (pdOffsets p)) classCommands

a1, a2, a3, a4, a5, a6, a7, a8 :: Period
[a1, a2, a3, a4, a5, a6, a7, a8]
  = zipWith (Period mwfOffsets [1,3,5])
      (map (("A"++) . show) [1..8])
      (map (\h -> time h 10) [8,9,10,11,12,1,2,3])

b1, b2, b3, b4 :: Period
[b1, b2, b3, b4] = zipWith (Period trOffsets [2,4])
  (map (("B"++) . show) [1..4])
  [time 8 15, time 9 45, time 1 15, time 2 45]

pdList :: [Period]
pdList = [a1,a2,a3,a4,a5,a6,a7,a8,b1,b2,b3,b4]

dowString :: DOW -> String
dowString = (["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]!!)

main :: IO ()
main = do
  (now, today) <- currentTime
  putStrLn $ "Today is " ++ dowString today ++ "."
  let thePd = head
            . sortBy (comparing (\p -> abs (pdStart p - now)))
            . filter (\p -> today `elem` pdDays p)
            $ pdList
  putStrLn $ "Closest period is " ++ pdName thePd ++ "."
  blinkIntervals (pdIntervals thePd)

