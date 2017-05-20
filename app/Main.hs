module Main where

import           Control.Monad.Logger     (runStderrLoggingT)
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))
import           Data.Text                (pack)
import           Database.Persist.Sqlite  (createSqlitePool, runMigration,
                                           runSqlPersistMPool)
import           Network.Wai.Handler.Warp (run)
import           Servant                  (serve)
import           System.Directory         (createDirectoryIfMissing)
import           System.Environment

import           API                      (api, server)
import           Persistence              (migrateAll)

main :: IO ()
main = do
    mPort <- lookupEnv "PORT"
    let port = fromMaybe 8080 (read <$> mPort)

    mDbPath <- lookupEnv "DB_PATH"
    let dbPath = fromMaybe "./db/" mDbPath

    createDirectoryIfMissing True dbPath

    pool <- runStderrLoggingT $ createSqlitePool (pack dbPath <> "db.sqlite3") 5
    flip runSqlPersistMPool pool $ runMigration migrateAll

    run port $ app pool
  where app pool = serve api $ server pool
