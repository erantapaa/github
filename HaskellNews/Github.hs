module HaskellNews.Github where

import Github.Util (GithubAuth(..),formatUTC,handleRateLimit,githubCreds,uniqueRepos)
import qualified Github.Data as Github
import qualified Github.Search as Github
import Data.Time.Clock (getCurrentTime,UTCTime,addUTCTime,NominalDiffTime(..))
import Control.Monad (forM,forM_)
import Data.List (intercalate)
import GHC.Exts (sortWith)
import Data.Maybe (fromMaybe,maybe)
import System.Environment (getArgs)
import System.Locale
import Data.Time
import qualified Data.Time.Format as DT


allPages :: Show a => (String -> IO (Either a Github.SearchReposResult)) -> String -> IO [[Github.Repo]]
allPages q baseQuery = go Nothing 1 []
  where
    go needed pageno results =
      if maybe False (<= 0) needed
        then return results
        else do
          reply <- q (baseQuery ++ "&per_page=100&page=" ++ show pageno)
          case reply of
            Left e -> do
              putStrLn $ "Error for page " ++ show pageno ++ ": " ++ show e
              return results
            Right repos -> do
              putStrLn $ "count for page " ++ show pageno ++ ": " ++ show count
              if count >= 100
                then go needed' (pageno+1) (page_repos:results)
                else return (page_repos:results)
              where
                needed' = Just $ maybe total_count (subtract count) needed
                total_count = Github.searchReposTotalCount repos
                page_repos = Github.searchReposRepos repos
                count = length page_repos

baseQuery :: String -> String  -> UTCTime -> String
baseQuery language field utcTime =
  let date = formatUTC utcTime
  in "q=" ++ "language%3A" ++ language ++ " " ++ field ++ "%3A>" ++ date 

doQuery :: Maybe GithubAuth -> String -> IO (Either Github.Error Github.SearchReposResult)
doQuery auth query = handleRateLimit $ Github.searchRepos' auth query

formatRepo :: Github.Repo -> String
formatRepo r =
  let fields = [ ("Name", Github.repoName)
                 ,("URL",  Github.repoHtmlUrl)
                 ,("Description", orEmpty . Github.repoDescription)
                 ,("Created-At", formatDate . Github.repoCreatedAt)
                 ,("Pushed-At", formatMaybeDate . Github.repoPushedAt)
               ]
  in intercalate "\n" $ map fmt fields
    where fmt (s,f) = fill 12 (s ++ ":") ++ " " ++ f r
          orEmpty = fromMaybe ""
          fill n s = s ++ replicate n' ' '
            where n' = max 0 (n - length s)

formatMaybeDate = maybe "???" formatDate

formatDate = show . Github.fromGithubDate

pushedRepos :: Maybe Github.GithubAuth -> UTCTime -> IO [ Github.Repo ]
pushedRepos auth t = do
  results <- allPages (doQuery auth) (baseQuery "haskell" "pushed" t)
  let repos = sortWith Github.repoPushedAt (uniqueRepos $ concat $ results)
  return repos

parseTime :: String -> IO UTCTime 
parseTime arg@('-':_) = do
  let delta = read arg :: Integer
  now <- getCurrentTime
  let diff = fromInteger $ delta
  let t = addUTCTime diff now
  return t
parseTime arg =
  let maybet = DT.parseTime defaultTimeLocale "%FT%T" arg in
  case maybet of
    Nothing -> error $ "bad time: " ++ arg
    Just t  -> return t

