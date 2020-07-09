-- API token: akNoVWHMShKz9cyhzMDJwSVX8c1qoe

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString      as S
import qualified Data.ByteString.UTF8 as U
import           System.IO.Streams    (InputStream, OutputStream, stdout)
import qualified System.IO.Streams    as Streams

import           Control.Applicative  (liftA2, (<$>))
import           Control.Monad        (when)
import           Data.Char            (isSpace)
import           Data.List            (isPrefixOf)
import           Data.Maybe           (fromJust, listToMaybe)
import           Data.Time
import           Network.Http.Client
import           OpenSSL              (withOpenSSL)
import           System.Directory     (doesFileExist, getHomeDirectory)
import           System.FilePath      ((</>))
import           System.Locale        (defaultTimeLocale)

bufferMinutes :: Num a => a
bufferMinutes = 10

main :: IO ()
main = do
  home <- getHomeDirectory
  curTime <- getCurrentTime
  let logDir     = "documents/logs/diagrams"
      notifyFile = home </> logDir </> "notify.log"
      logFile    = home </> logDir </> "diagrams.log"
  (numLinesSeen, lastMsgTime) <- getLast curTime notifyFile
  logLines <- lines <$> readFile logFile
  let newLog  = drop numLinesSeen logLines
      numLinesSeen' = numLinesSeen + length newLog
      mnewMsg = listToMaybe $ filter isMsg newLog
      timeElapsed = diffUTCTime curTime lastMsgTime

  case mnewMsg of
    Nothing -> recordLast notifyFile numLinesSeen' lastMsgTime
    Just newMsg -> do
      when (timeElapsed >= bufferMinutes * 60) $ notify newMsg
      recordLast notifyFile numLinesSeen' curTime

isMsg :: String -> Bool
isMsg = (fmap and . sequence $
          [ (`elem` ["<", "*"]) . take 1
          , not . isPrefixOf "< travis-ci>"
          , not . isPrefixOf "<   byorgey>"
          ]
        )
      . (drop 1 . dropWhile (not . isSpace))

recordLast :: FilePath -> Int -> UTCTime -> IO ()
recordLast notifyFile n t = writeFile notifyFile (unlines [show n, show t])

getLast :: UTCTime -> FilePath -> IO (Int, UTCTime)
getLast curTime notifyFile = do
  e <- doesFileExist notifyFile
  when (not e) $ do
    writeFile notifyFile (unlines ["0", show curTime])

  s <- readFile notifyFile
  let [numLinesStr, timeStr] = lines s
      numLines = read numLinesStr :: Int
      time     = fromJust $ parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q UTC" timeStr
  return (numLines, time)

notify :: String -> IO ()
notify msg = withOpenSSL $ do
    ctx <- baselineContextSSL
    c <- openConnectionSSL ctx "api.pushover.net" 443

    q <- buildRequest $ do
        http POST "/1/messages.json"
        setAccept "application/json"

    let params =
          [ ("token", "akNoVWHMShKz9cyhzMDJwSVX8c1qoe")
          , ("user", "uRPtLGMb1of9rmpae43UVztymUYZc3")
          , ("message", U.fromString msg)
          ]

    sendRequest c q (encodedFormBody params)

    receiveResponse c (\_ i -> do
        xm <- Streams.read i
        case xm of
            Just x    -> S.putStr x
            Nothing   -> return ())

    closeConnection c
