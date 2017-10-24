{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Maybe         (fromMaybe)
import           System.Environment (lookupEnv)

import           Persistence
import           Server

main :: IO ()
main = do
    mPort <- lookupEnv "PORT"
    let port = fromMaybe 8080 (read <$> mPort)
    let defaultDbConnection = "host=127.0.0.1 user=postgres dbname=postgres port=5432"
    mDbConnection <- lookupEnv "DB_CONNECTION"
    let dbConnection = fromMaybe defaultDbConnection mDbConnection

    runDBMigration dbConnection
    startServer port dbConnection
