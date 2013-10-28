{-# LANGUAGE OverloadedStrings #-}
module Github.Util (
  Github.GithubAuth(..),
  githubCreds,
  sleepSeconds,
  today,
  rateLimit,
  handleRateLimit,
  formatUTC,
  uniqueRepos
) where

import qualified Github.Private as Github
import qualified Github.Data as Github
import qualified Data.ByteString as BS
import System.Posix.Env.ByteString (getEnv)
import qualified Data.Char as C
import Data.Word (Word8)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, UTCTime(..))
import Text.Printf (printf)
import Data.Time.LocalTime (utc,utcToLocalTime,localDay,localTimeOfDay,TimeOfDay(..))
import Data.Time.Calendar (toGregorian)
import qualified Control.Exception as E
import qualified Network.HTTP.Types as W
import Data.Maybe (fromMaybe,listToMaybe)
import Network.HTTP.Conduit (HttpException(..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Exts (sortWith,groupWith)

data RateLimit = RateLimit { rateLimitLimit :: Int, rateLimitRemaining :: Int, rateLimitReset :: Int } deriving (Show)

-- | return credentials found in GITHUB_CREDENTIALS
githubCreds :: IO (Maybe Github.GithubAuth)
githubCreds = do
  c <- getEnv "GITHUB_CREDENTIALS"
  return $ case c of
    Nothing -> Nothing
    Just s  ->
      let (user,rest) = BS.breakSubstring "," s
          pw = BS.drop 1 rest
       in Just $ Github.GithubBasicAuth user pw

-- | sleep for a number of seconds
sleepSeconds :: Int -> IO()
sleepSeconds n = threadDelay (n*1000000)

-- | return today (in UTC) formatted as YYYY-MM-DD
today :: IO String
today = do
  now <- getCurrentTime
  let day = localDay $ utcToLocalTime utc now
      (y,m,d) = toGregorian day
   in return $ printf "%d-%02d-%02d" y m d

-- | perform an IO operation handling rate limit errors
handleRateLimit io = do
  result <- io
  case result of
    Left e  ->
      case rateLimit e of
        Nothing -> return result
        Just rl -> do putStrLn $ "rate limited: " ++ show rl
                      now <- getCurrentTime
                      let diff = rateLimitReset rl - (toSecs now)
                      putStrLn $ "need to sleep for " ++ show diff
                      sleepSeconds diff
                      handleRateLimit io
    otherwise -> return result

-- | return the rate limit details from an error
rateLimit :: Github.Error -> Maybe RateLimit
rateLimit e =
  case e of
    Github.HTTPConnectionError except ->
      case E.fromException except of
        Just (StatusCodeException (W.Status 403 _) headers _) -> Just $ RateLimit limit remaining reset
          where limit     = findHeader headers "X-RateLimit-Limit"
                remaining = findHeader headers "X-RateLimit-Remaining"
                reset     = findHeader headers "X-RateLimit-Reset"
        Just _ -> Nothing
        Nothing -> Nothing
    otherwise -> Nothing

findHeader hs key =
  case lookup key hs of
    Nothing -> 0
    Just x  -> fromMaybe 0 (maybeRead (map toChar $ BS.unpack x))

-- | Attempts to parse a value from the front of the string.
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | blah blah
toChar :: Word8 -> Char
toChar = C.chr . fromIntegral

-- | convert a UTCTime to Unix epoch time (integral)
toSecs :: UTCTime -> Int
toSecs = round . utcTimeToPOSIXSeconds

-- | format a UTCTime
formatUTC :: UTCTime -> String
formatUTC t =
  let local = utcToLocalTime utc t
      day = localDay local
      (y,m,d) = toGregorian day
      localtime = localTimeOfDay local
      hh = todHour localtime
      mm = todMin localtime
      ss = todSec localtime
   in printf "%d-%02d-%02dT%02d:%02d:%02d" y m d hh mm ((floor ss) :: Int)

-- | remove duplicates from a Repo list
uniqueRepos :: [ Github.Repo ] -> [ Github.Repo ]
uniqueRepos rs =
  let sorted = sortWith Github.repoHtmlUrl rs
      grouped = groupWith Github.repoHtmlUrl sorted
  in map head grouped

