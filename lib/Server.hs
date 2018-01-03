{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import           Control.Monad.Reader       (runReaderT)
import           Data.Pool                  (Pool)
import           Database.PostgreSQL.Simple (Connection)
import           Network.Wai.Handler.Warp   (run)
import           Servant

import           API
import           Persistence
import           Types

nt :: Env -> AppM a -> Handler a
nt s x = runReaderT x s

app :: Env -> Application
app s = serve api $ hoistServer api (nt s) server

startServer :: Integer -> Pool Connection -> IO ()
startServer port pool = do
    runMigrations pool
    run (fromInteger port) $ app $ Env pool
