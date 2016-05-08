import           Control.Monad.Random
import           Data.Time
import           Doomsday

doomsdayAnchorTest :: IO (Bool, NominalDiffTime)
doomsdayAnchorTest
  = runTest
      (getRandomR (0,99))
      show
      (\guess yy -> let ans = ((yy :: Int) + yy `div` 4) `mod` 7
                    in  (read guess `mod` 7 == ans, show ans))

main :: IO ()
main = runTests doomsdayAnchorTest
