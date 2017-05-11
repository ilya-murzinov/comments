{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Control.Monad.IO.Class (liftIO)
import           Database.Persist
import           Database.Persist.Sql   (ConnectionPool, runSqlPersistMPool)

import           Models
import           Servant

type GetCommentsEndpoint = Capture "threadId" ThreadId :> Get  '[JSON] [Comment]
type PostCommentEndpoint = Capture "threadId" ThreadId :> ReqBody '[JSON] Comment :> PostCreated '[JSON] ()

type API = "api" :> "comments" :>
  (
       GetCommentsEndpoint
  :<|> PostCommentEndpoint
  )

api :: Proxy API
api = Proxy

getComments :: ConnectionPool -> ThreadId -> Handler [Comment]
getComments pool tid = liftIO $ flip runSqlPersistMPool pool $ do
  mComments <- selectList [CommentThreadId ==. tid] []
  return $ entityVal <$> mComments

addComment :: ConnectionPool -> ThreadId -> Comment -> Handler ()
addComment pool tid c = liftIO $ flip runSqlPersistMPool pool $ do
  _ <- insert c{commentThreadId = tid}
  return ()

server :: ConnectionPool -> Server API
server pool = getComments pool :<|> addComment pool
