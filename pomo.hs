{-# LANGUAGE GADTs #-}

module Pomo where

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
    callProcess "/home/brent/.local/bin/blink1-tool" (blinkArgs c)
  where
    blinkArgs Off           = ["--off"]
    blinkArgs (On clr)      = ["--rgb", showColor clr]
    blinkArgs (Blink clr n) = blinkArgs (On clr) ++ ["--blink=" ++ show n]
    debugMsg Off = "Turning off..."
    debugMsg (On clr) = "Changing to color " ++ show clr ++ "..."
    debugMsg (Blink clr n) = "Blinking color " ++ show clr ++ " " ++ show n ++ " times..."

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

main :: IO ()
main = pomo

pomoIntervals :: Minutes -> BlinkIntervals
pomoIntervals start = map (first (+start)) [(0, On green), (25, Blink cyan 10), (26, On cyan), (30, Blink green 10)]

pomo :: IO ()
pomo = do
  putStrLn $ "Starting pomo..."
  start <- fst <$> currentTime
  blinkIntervals (pomoIntervals start)

