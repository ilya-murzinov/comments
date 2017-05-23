{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Logger        (runStderrLoggingT)
import           Data.ByteString.Char8       (pack)
import           Data.Maybe                  (fromMaybe)
import           Database.Persist.Postgresql (createPostgresqlPool,
                                              runMigration, runSqlPersistMPool)
import           Network.Wai.Handler.Warp    (run)
import           Servant                     (serve)
import           System.Environment          (lookupEnv)

import           API                         (api, server)
import           Persistence

main :: IO ()
main = do
    mPort <- lookupEnv "PORT"
    let port = fromMaybe 8080 (read <$> mPort)

    mDbConnection <- lookupEnv "DB_CONNECTION"
    let dbConnection = pack $ fromMaybe "postgres://admin:admin@localhost:5432/hcomments" mDbConnection

    pool <- runStderrLoggingT $ createPostgresqlPool dbConnection 5
    flip runSqlPersistMPool pool $ runMigration migrateAll

    run port $ app pool
  where app pool = serve api $ server pool
