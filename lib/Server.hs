{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import           Database.PostgreSQL.Simple (Connection)
import           Network.Wai.Handler.Warp   (run)
import           Servant                    (serve)

import           API                        (api, server)
import           Persistence

startServer :: Integer -> Connection -> IO ()
startServer port conn = do
    runMigrations conn
    run (fromInteger port) $ serve api server
