{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import           Data.Maybe                   (fromMaybe)
import           Database.PostgreSQL.Embedded (DBConfig (..),
                                               StartupConfig (..), Version (..),
                                               startPostgres)
import           System.Environment           (lookupEnv)

import           Persistence
import           Server

main :: IO ()
main = do
    mPort <- lookupEnv "PORT"
    let port = fromMaybe 8080 (read <$> mPort)

    mDbConnection <- lookupEnv "DB_CONNECTION"
    case mDbConnection of
      Just _ -> return ()
      Nothing -> do
        let sConfig = StartupConfig True (Version "9.6.5-1")
        let dConfig = DBConfig 46782 "postgres"
        _ <- startPostgres sConfig dConfig
        return ()

    let dbConnection = fromMaybe "host=127.0.0.1 user=postgres dbname=postgres port=46782" mDbConnection

    runDBMigration dbConnection

    startServer port dbConnection
