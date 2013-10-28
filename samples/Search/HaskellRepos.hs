module HaskellRepos where

import Github.Util (GithubAuth(..),formatUTC,handleRateLimit,githubCreds,uniqueRepos)
import qualified Github.Data as Github
import qualified Github.Search as Github
import Data.Time.Clock (getCurrentTime,UTCTime,addUTCTime,NominalDiffTime(..))
import Control.Monad (forM,forM_)
import Data.List (intercalate)
import GHC.Exts (sortWith)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

repoQuery q letters = forM letters (\letter -> do
   results <- q letter
   case results of
     Left e  -> do putStrLn $ "Error for letter '" ++ [letter] ++ "': " ++ show e;
                   return []
     Right r -> do putStrLn $ "letter " ++ [letter] ++ " count: " ++ show n
                   return $ Github.searchReposRepos r
       where n = Github.searchReposTotalCount r
   )

languageQuery :: Maybe GithubAuth -> String -> String  -> UTCTime -> Char -> IO (Either Github.Error Github.SearchReposResult)
languageQuery auth language field utcTime letter = 
  let query = "q=" ++ [letter] ++ " in%3Aname language%3A" ++ language ++ " " ++ field ++ "%3A>" ++ date ++ "&per_page=100"
      date = formatUTC utcTime
  in do
     -- putStrLn $ "=== query: " ++ query
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

parseArgs :: IO (String,Int)
parseArgs = do
  args <- getArgs
  return $ case args of
    [('c':_)] -> ("created",86400)
    [('p':_)] -> ("pushed",86400)
    (('c':_):digits:_) -> ("created", read digits)
    (('p':_):digits:_) -> ("pushed", read digits)
    _ -> error "bad usage"

main :: IO ()
main = do
  (field, delta) <- parseArgs
  auth <- githubCreds
  now <- getCurrentTime
  let diff = fromInteger $ toInteger $ (negate delta)
  let t = addUTCTime diff now
  -- putStrLn $ "delta = " ++ show delta
  -- putStrLn $ "now   = " ++ show now
  -- putStrLn $ "t     = " ++ show t
  let q = languageQuery auth "haskell" field t 
  results <- repoQuery q ['a'..'z']
  let repos = sortWith Github.repoPushedAt (uniqueRepos $ concat $ results)
  forM_ repos (\r -> do
    putStrLn $ formatRepo r
    putStrLn ""
    )

