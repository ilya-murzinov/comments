{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Persistence
    ( createDefaultPool
    , runMigrations
    , getThread
    , createThread
    , getComments
    , addComment
    ) where

import           Control.Monad                        (void)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Reader                 (ask)
import qualified Data.ByteString.Char8                as BS
import           Data.FileEmbed                       (embedDir)
import           Data.Foldable                        (forM_)
import           Data.Function                        (on)
import           Data.List                            (sortBy)
import           Data.Monoid                          ((<>))
import           Data.Pool                            (Pool, createPool,
                                                       withResource)
import           Data.Time                            (UTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple           (Connection, Only (..),
                                                       connectPostgreSQL,
                                                       execute, query, query_)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (..),
                                                       MigrationContext (..),
                                                       MigrationResult (..),
                                                       runMigration)
import           System.Exit                          (exitFailure)
import           System.Log.Logger

import           Types
import           Utils

comp = "HComments.Persistence"

createDefaultPool :: String -> IO (Pool Connection)
createDefaultPool connStr = createPool (connectPostgreSQL $ BS.pack connStr) (\_ -> return ()) 1 5 1

instance FromRow Thread where
    fromRow = Thread <$> field <*> field <*> field

sortedMigrations :: [(FilePath, BS.ByteString)]
sortedMigrations =
    let unsorted = $(embedDir "db/migrations")
    in sortBy (compare `on` fst) unsorted

runMigrations :: Pool Connection -> IO ()
runMigrations pool = withResource pool $ \conn ->
  do
    warningM comp $ "Applying migrations..."
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
        warningM comp $ "Applying migration " <> migrDescr
        res <- runMigration migr
        case res of
            MigrationSuccess -> return ()
            MigrationError reason -> do
                errorM comp $ "Migration failed: " <> reason
                exitFailure

getThread :: ThreadId -> AppM (Maybe Thread)
getThread (ThreadId tid) = do
    Env{pool = p} <- ask
    liftIO $ withResource p $ \conn -> do
      let param = (Only ((fromInteger tid) :: Int))
      (threads :: [Thread]) <- query conn "select id, title, created from threads where id = ?" param
      return $ case threads of
        [t] -> Just t
        _   -> Nothing

createThread :: PartialThread -> AppM Thread
createThread (PartialThread title) = do
    Env{pool = p} <- ask
    now <- liftIO getCurrentTime
    liftIO $ withResource p $ \conn -> do
      [t@Thread{}] <- query conn "insert into threads (title, created) values (?, ?) returning id, title, created" (title, now)
      return t

getComments :: ThreadId -> AppM [Comment]
getComments = undefined

addComment :: ThreadId -> PartialComment -> AppM (Maybe Comment)
addComment = undefined
