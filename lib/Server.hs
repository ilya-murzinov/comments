{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import           Data.Pool                  (Pool)
import           Database.PostgreSQL.Simple (Connection)
import           Network.Wai.Handler.Warp   (run)
import           Servant

import           API
import           Persistence

startServer :: Integer -> Pool Connection -> IO ()
startServer port pool = do
    runMigrations pool
    run (fromInteger port) $ serve api server
