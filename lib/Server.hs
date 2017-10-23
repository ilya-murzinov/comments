{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import           Data.ByteString.Char8    (pack)
import           Network.Wai.Handler.Warp (run)
import           Servant                  (serve)

import           API                      (api, server)
import           Persistence

startServer :: Integer -> String -> IO ()
startServer port connectionString = run (fromInteger port) $ serve api server
