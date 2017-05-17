{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Persistence where

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.List               (find)
import           Data.Maybe              (catMaybes)
import           Data.Text               (Text)
import           Data.Time               (UTCTime, getCurrentTime)
import           Database.Persist.Sqlite
import           Database.Persist.TH

import           Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
PersistentUser sql=users
    name Text
    email Email
    UniqueEmail email
    deriving Show Read
PersistentThread sql=threads
    title Text
    created UTCTime
    deriving Show Read
PersistentComment sql=comments
    userId PersistentUserId
    threadId PersistentThreadId
    created UTCTime
    parentId PersistentCommentId Maybe
    title Text
    body Text
    deriving Show Read
|]

getComments :: MonadIO m => ConnectionPool -> ThreadId -> m [Comment]
getComments pool (ThreadId tid) = liftIO $ flip runSqlPersistMPool pool $ do
  let dbtid = toSqlKey tid :: PersistentThreadId
  mComments <- selectList [PersistentCommentThreadId ==. dbtid] []
  mUsers <- selectList [PersistentUserId <-. (persistentCommentUserId . entityVal <$> mComments)] []
  return $ fromPersistentLists mComments mUsers

addComment :: MonadIO m => ConnectionPool -> ThreadId -> PartialComment -> m Comment
addComment pool (ThreadId tid) (PartialComment n e t b mP) = liftIO $ flip runSqlPersistMPool pool $ do
  let dbtid = toSqlKey tid :: PersistentThreadId
  let u = PersistentUser n e
  mUser <- getBy $ UniqueEmail e
  uid <- case mUser of
    Just (Entity existingUid _) -> return existingUid
    Nothing -> insert u
  now <- liftIO getCurrentTime
  let c = PersistentComment uid dbtid now (toSqlKey <$> mP) t b
  cid <- insert c
  return $ fromPersistent (Entity cid c) (Entity uid u)

fromPersistent :: Entity PersistentComment -> Entity PersistentUser -> Comment
fromPersistent (Entity cid (PersistentComment _ _ _ _ t _)) (Entity _ (PersistentUser name email)) =
  Comment (fromSqlKey cid) t name email

fromPersistentLists :: [Entity PersistentComment] -> [Entity PersistentUser] -> [Comment]
fromPersistentLists comments users =
  catMaybes $
  fmap
    (\comment ->
       fromPersistent comment <$>
       find
         (\(Entity uid _) -> uid == persistentCommentUserId (entityVal comment))
         users)
    comments
