{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import           Control.Monad.Logger        (runStderrLoggingT)
import           Data.ByteString.Char8       (pack)
import           Database.Persist.Postgresql (createPostgresqlPool,
                                              runMigration, runSqlPersistMPool)
import           Network.Wai.Handler.Warp    (run)
import           Servant                     (serve)

import           API                         (api, server)
import           Persistence

startServer :: Integer -> String -> IO ()
startServer port connectionString = do
    pool <- runStderrLoggingT $ createPostgresqlPool (pack connectionString) 5
    flip runSqlPersistMPool pool $ runMigration migrateAll

    run (fromInteger port) $ app pool
  where app pool = serve api $ server pool
