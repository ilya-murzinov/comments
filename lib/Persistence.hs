{-# LANGUAGE TemplateHaskell #-}

module Persistence where

import           Control.Monad                        (void)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import qualified Data.ByteString                      as BS
import           Data.FileEmbed                       (embedDir)
import           Data.Foldable                        (forM_)
import           Data.Function                        (on)
import           Data.List                            (sortBy)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Migration

import           Types
import           Utils

instance FromRow Thread where
    fromRow = Thread <$> field <*> field <*> field

sortedMigrations :: [(FilePath, BS.ByteString)]
sortedMigrations =
    let unsorted = $(embedDir "db/migrations")
    in sortBy (compare `on` fst) unsorted

runMigrations :: Connection -> IO ()
runMigrations conn =
  do
    let defaultContext =
          MigrationContext
          { migrationContextCommand = MigrationInitialization
          , migrationContextVerbose = False
          , migrationContextConnection = conn
          }
        migrations = ("(init)", defaultContext) :
                     [
                        (k, defaultContext
                            { migrationContextCommand =
                                MigrationScript k v
                            })
                        | (k, v) <- sortedMigrations
                     ]
    forM_ migrations $ \(migrDescr, migr) -> do
      res <- runMigration migr
      return ()

getThread :: MonadIO m => Connection -> ThreadId -> m (Maybe Thread)
getThread conn (ThreadId tid) = liftIO $ Just <$> do
    [t@Thread{}] <- query_ conn "select id, title, created from threads"
    return t

-- import           Control.Monad.IO.Class      (MonadIO, liftIO)
-- import           Data.Text                   (Text)
-- import           Data.Time                   (UTCTime, getCurrentTime)
-- import           Database.Persist.Postgresql
-- import           Database.Persist.TH

-- import           Types

-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- PersistentUser sql=users
--   name Text
--   email Email
--   UniqueEmail email
--   deriving Show Read
-- PersistentThread sql=threads
--   identifier Text
--   title Text Maybe
--   created UTCTime
--   UniqueIdentifier identifier
--   deriving Show Read
-- PersistentComment sql=comments
--   userId PersistentUserId
--   threadId PersistentThreadId
--   created UTCTime
--   parentId PersistentCommentId Maybe
--   title Text Maybe
--   body Text
--   deriving Show Read
-- |]

-- getThread :: MonadIO m => ConnectionPool -> ThreadIdentifier -> m (Maybe Thread)
-- getThread pool (ThreadIdentifier tid) = liftIO $ flip runSqlPersistMPool pool $ do
--   thread <- getBy $ UniqueIdentifier tid
--   return $ fromPersistentThread <$> thread

-- createThread :: MonadIO m => ConnectionPool -> PartialThread -> m Thread
-- createThread pool (PartialThread i title) = liftIO $ flip runSqlPersistMPool pool $ do
--   now <- liftIO getCurrentTime
--   let t = PersistentThread i title now
--   tid <- insert t
--   return $ fromPersistentThread $ Entity tid t

-- getComments :: MonadIO m => ConnectionPool -> ThreadId -> m [Comment]
-- getComments pool (ThreadId tid) = liftIO $ flip runSqlPersistMPool pool $ do
--   let sql = "select ??,?? from comments join users on comments.user_id = users.id where comments.thread_id = ?;"
--   pairs <- rawSql sql [PersistInt64 tid]
--   return $ fromPair <$> pairs
--   where fromPair (f, s) = fromPersistent f s

-- addComment :: MonadIO m => ConnectionPool -> ThreadId -> PartialComment -> m (Maybe Comment)
-- addComment pool (ThreadId tid) (PartialComment n e t b mP) = liftIO $ flip runSqlPersistMPool pool $ do
--   let dbtid = toSqlKey tid :: PersistentThreadId
--   mThread <- get dbtid
--   case mThread of
--     Nothing -> return Nothing
--     Just _ -> do
--       let u = PersistentUser n e
--       mUser <- getBy $ UniqueEmail e
--       uid <- case mUser of
--         Just (Entity existingUid _) -> return existingUid
--         Nothing                     -> insert u
--       now <- liftIO getCurrentTime
--       let c = PersistentComment uid dbtid now (toSqlKey <$> mP) t b
--       cid <- insert c
--       return $ Just $ fromPersistent (Entity cid c) (Entity uid u)

-- fromPersistent :: Entity PersistentComment -> Entity PersistentUser -> Comment
-- fromPersistent (Entity cid (PersistentComment _ _ c _ _ b)) (Entity _ (PersistentUser name email)) =
--   Comment (fromSqlKey cid) c b name email

-- fromPersistentThread :: Entity PersistentThread -> Thread
-- fromPersistentThread (Entity key (PersistentThread i mT c)) = Thread (fromSqlKey key) i mT c
