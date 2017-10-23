{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Logger         (runStderrLoggingT)
import           Data.ByteString.Char8        (pack)
import           Data.Maybe                   (fromMaybe)
import           Database.Persist.Postgresql  (createPostgresqlPool,
                                               runMigration, runSqlPersistMPool)
import           Database.PostgreSQL.Embedded
import           Network.Wai.Handler.Warp     (run)
import           Servant                      (serve)
import           System.Environment           (lookupEnv)

import           API                          (api, server)
import           Persistence

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

    let dbConnection = pack $ fromMaybe "host=127.0.0.1 user=postgres dbname=postgres port=46782" mDbConnection

    pool <- runStderrLoggingT $ createPostgresqlPool dbConnection 5
    flip runSqlPersistMPool pool $ runMigration migrateAll

    run port $ app pool
  where app pool = serve api $ server pool
