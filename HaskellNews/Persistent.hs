{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module HaskellNews.Persistent where

import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (rawQuery, insert, repsert, insertBy)
import Data.Conduit (($$))
import Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import Database.Persist.Sqlite (runMigration)
import Control.Monad (forM_)
import qualified Github.Data as Github
import qualified Github.Search as Github
import Data.Maybe (fromMaybe,maybe)
import Github.Util (formatUTC)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
PushEvent
   repoId    Text
   eventTime Text
   repoName  Text
   repoDescription Text
   repoUrl   Text
   PrimaryKey eventTime repoId
   deriving Show
|]

runDB_ = runSqlite

doMigration = runMigration migrateTables

doDump = rawQuery "select * from push_event" [] $$ CL.mapM_ (liftIO . print)

doCount = rawQuery "select count(*) from push_event" [] $$ CL.mapM_ (liftIO . print)

toPushEvent repo = 
  let repo_id    = T.pack $ show $ Github.repoId repo
      event_time = T.pack $ maybe "" (formatUTC . Github.fromGithubDate) $ Github.repoPushedAt repo
      repo_name  = T.pack $ Github.repoName repo
      repo_desc  = T.pack $ fromMaybe "" $ Github.repoDescription repo
      repo_url   = T.pack $ Github.repoUrl repo
  in PushEvent repo_id event_time repo_name repo_desc repo_url

doInsert repo = insertBy $ toPushEvent repo

