module Main where

import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Logger
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH      ()
import           Network.Wai.Handler.Warp (run)
import           Servant                  (serve)
import           System.Directory         (createDirectoryIfMissing)

import           API
import           Models
import           Types

main :: IO ()
main = do
    createDirectoryIfMissing True "db"
    pool <- runStderrLoggingT $ createSqlitePool "db/db.sqlite3" 5
    flip runSqlPersistMPool pool $ do
      runMigration migrateAll

      johnId <- insert $ User "John Doe" (Email "asd@asd.asd")

      john <- get johnId
      liftIO $ print (john :: Maybe User)

      now <- liftIO getCurrentTime
      threadId <- insert $ Thread "Thread 1" now

      commentId <- insert $ Comment johnId threadId now Nothing "title" "text"

      c <- get commentId
      liftIO $ print (c :: Maybe Comment)

    run 8080 $ app pool
  where app pool = serve api $ server pool
