module Main where

import           Data.Maybe         (fromMaybe)
import           Data.Monoid        ((<>))
import           System.Environment (lookupEnv)
import           System.Log.Logger

import           Persistence
import           Server

main :: IO ()
main = do
    let comp = "HComments.Main"

    mPort <- lookupEnv "PORT"
    let port = fromMaybe 8080 (read <$> mPort)

    let defaultDbConnection = "host=127.0.0.1 user=postgres dbname=postgres port=5432"
    mDbConnection <- lookupEnv "DB_CONNECTION"
    let dbConnection = fromMaybe defaultDbConnection mDbConnection

    warningM comp $ "Connecting to DB with " <> dbConnection
    pool <- createDefaultPool dbConnection

    warningM comp $ "Starting server on port " <> show port
    startServer port pool
