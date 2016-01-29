{-# LANGUAGE GADTs #-}

module Blink where

import           Control.Arrow      (first)
import           Control.Concurrent (threadDelay)
import           Control.Monad      (when)
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.SRGB
import           Data.List          (find)
import           Data.Time
import           System.Environment (getArgs)
import           System.Process
import           Text.Printf

{- To do:

   * Change intervals type to use hours + minutes instead of seconds, and
     convert internally
   * Fix greater-than-or-equal-to bug?
   * Debugging output
-}


data BlinkCommand
  = Off
  | On (Colour Double)
  | Quit

colorToHex :: Colour Double -> String
colorToHex c = hex r ++ hex g ++ hex b
  where
    RGB r g b = toSRGB24 c
    hex = printf "%02x"

runBlinkCommand :: BlinkCommand -> IO ()
runBlinkCommand Quit = runBlinkCommand Off
runBlinkCommand c    = callProcess "blink1-tool" (blinkArgs c)
  where
    blinkArgs Off      = ["--off"]
    blinkArgs (On clr) = ["--rgb=" ++ colorToHex clr]

-- | Seconds since midnight
type Seconds = Int

-- Invariant: should be sorted by time
type BlinkIntervals = [(Seconds, BlinkCommand)]

blinkIntervals :: BlinkIntervals -> IO ()
blinkIntervals intervals = go Nothing
  where
    go current = do
      delay 1
      secs <- currentSeconds
      let int = findInterval secs intervals
      case int of
        Nothing -> go current
        Just (time, cmd) -> do
          when (Just time /= current) $ runBlinkCommand cmd
          case cmd of
            Quit -> return ()
            _    -> go $ Just time

currentSeconds :: IO Seconds
currentSeconds = do
  tod <- (localTimeOfDay . zonedTimeToLocalTime) <$> getZonedTime
  return $ todHour tod * 60 + todMin tod

findInterval :: Seconds -> BlinkIntervals -> Maybe (Seconds, BlinkCommand)
findInterval secs = safeLast . takeWhile ((<= secs) . fst)
  where
    safeLast [] = Nothing
    safeLast xs = Just $ last xs

delay :: Int -> IO ()
delay secs = threadDelay (1000000 * secs)

twoMinutes :: BlinkIntervals
twoMinutes = [(784, On red), (785, On blue), (786, Quit)]

classCommands = [On cyan, On green, On blue, On purple, On yellow, On red, Quit]

fp :: BlinkIntervals
fp = zip [794, 795, 840, 855, 865, 868, 870] classCommands

algo :: BlinkIntervals
algo = zip [489, 490, 515, 525, 535, 538, 540] classCommands

intro :: BlinkIntervals
intro = (map . first) (+180) algo

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["fp"]    -> blinkIntervals fp
    ["algo"]  -> blinkIntervals algo
    ["intro"] -> blinkIntervals intro
    _         -> putStrLn "Please choose fp, algo, or intro"
