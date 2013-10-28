{-# LANGUAGE OverloadedStrings #-}
module SearchRepos where

import Github.Util (today,githubCreds,handleRateLimit)
import qualified Github.Search as Github
import qualified Github.Data as Github
import Control.Monad (forM,forM_)
import Data.Maybe (fromMaybe)

import Data.List (intercalate,sortBy,groupBy)
import GHC.Exts (sortWith,groupWith)

main = do
  results <- todaysUpdates
  print $ length results

main2 = do
  results <- todaysUpdates
  let names = map Github.repoName (uniqueRepos results)
  print names

main3 = do
  results <- todaysUpdates
  let repos = sortWith Github.repoPushedAt (uniqueRepos results)
  forM_ repos (\r -> do
    putStrLn $ formatRepo r
    putStrLn ""
    )

-- | remove duplicates from a Repo list
uniqueRepos :: [ Github.Repo ] -> [ Github.Repo ]
uniqueRepos rs = 
  let sorted = sortWith Github.repoHtmlUrl rs
      grouped = groupWith Github.repoHtmlUrl sorted
  in map head grouped

todaysUpdates = do
  day <- today
  auth <- githubCreds
  results <- haskellUpdates auth day
  return $ concat results

test1 = haskellUpdates Nothing "2013-10-27"

test2 = do
  auth <- githubCreds
  haskellUpdates auth "2013-10-27"

haskellUpdates auth date = forM ['a'..'e'] (\letter -> do
   results <- haskellUpdates' auth date letter
   case results of
     Left e  -> do putStrLn $ "Error for letter '" ++ [letter] ++ "': " ++ show e;
                   return []
     Right r -> do putStrLn $ "letter " ++ [letter] ++ " count: " ++ show n
                   return $ Github.searchReposRepos r
       where n = Github.searchReposTotalCount r
   )

haskellUpdates' :: Maybe Github.GithubAuth -> String -> Char -> IO (Either Github.Error Github.SearchReposResult)
haskellUpdates' auth date letter = do
  let query = "q=" ++ [letter] ++ " in%3Aname language%3Ahaskell pushed%3A>" ++ date ++ "&per_page=100"
  handleRateLimit $ Github.searchRepos' auth query

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

