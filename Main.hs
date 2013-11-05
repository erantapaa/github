{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import System.Environment (getArgs)
import qualified HaskellNews.Github as HN
import qualified HaskellNews.Persistent as HN
import Github.Util (GithubAuth(..),githubCreds,uniqueRepos)
import Data.Time.Clock (UTCTime) 
import Control.Monad (forM_)
import System.Directory (removeFile)
import qualified Control.Exception as CE
import System.IO.Error (isDoesNotExistError)

main = getArgs >>= doArgs

doArgs :: [String] -> IO ()
doArgs []                  = return ()
doArgs ("migrate":as)      = do { cmdMigrate; doArgs as }
doArgs ("dump":as)         = do { cmdDump; doArgs as }
doArgs ("count":as)        = do { cmdCount; doArgs as }
doArgs ("updates":time:as) = do { cmdUpdates time; doArgs as }
doArgs ("load":time:as)    = do { cmdLoad time; doArgs as }
doArgs ("html":time:as)    = do { cmdHtml time; doArgs as }
doArgs ("reset":as)        = do { cmdReset; doArgs as }
doArgs (cmd:_)             = error $ "unknown command: " ++ cmd 

language = "haskell"

dbName = "events.db"

runDB = HN.runDB_ dbName

cmdReset = do
  removeIfExists dbName
  cmdMigrate

cmdMigrate = runDB HN.doMigration

cmdDump = runDB HN.doDump

cmdCount = runDB HN.doCount

cmdUpdates time = do
  auth <- githubCreds
  t <- HN.parseTime time
  repos <- HN.pushedRepos auth language t
  forM_ repos (\r -> do
    putStrLn $ HN.formatRepo r
    putStrLn ""
    )
  putStrLn $ "count: " ++ show (length repos)

cmdLoad time = do
  auth <- githubCreds
  t <- HN.parseTime time
  repos <- HN.pushedRepos auth language t
  if null repos
    then putStrLn "No events to load."
    else do runDB $ forM_ repos HN.doInsert
            putStrLn $ "Events loaded: " ++ show (length repos)

cmdHtml time = do
  auth <- githubCreds
  t <- HN.parseTime time
  repos <- HN.pushedRepos auth language t
  forM_ repos (\r -> do
    putStrLn $ HN.formatRepoHtml r
    putStrLn ""
    )

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `CE.catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = CE.throwIO e

