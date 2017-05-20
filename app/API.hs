{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Database.Persist.Sql (ConnectionPool)
import           Servant

import           Persistence
import           Types

type TID = Capture "threadId" ThreadId
type GetCommentsEndpoint = TID :> Get '[JSON] [Comment]
type PostCommentEndpoint = TID :> ReqBody '[JSON] PartialComment :>
                                  PostCreated '[JSON] Comment
type GetThreadEndpoint = Capture "threadIdentifier" ThreadIdentifier :> Get '[JSON] Thread
type CreateThreadEndpoint = ReqBody '[JSON] PartialThread :> PostCreated '[JSON] Thread

type API = "api" :>
  (    "threads" :> GetThreadEndpoint
  :<|> "threads" :> CreateThreadEndpoint
  :<|> "comments" :> GetCommentsEndpoint
  :<|> "comments" :> PostCommentEndpoint
  )

api :: Proxy API
api = Proxy

getThreadEndpoint :: ConnectionPool -> ThreadIdentifier -> Handler Thread
getThreadEndpoint pool tid = do
  mThread <- getThread pool tid
  case mThread of
    Just thread -> return thread
    Nothing     -> throwError err404

createThreadEndpoint :: ConnectionPool -> PartialThread -> Handler Thread
createThreadEndpoint = createThread

getCommentsEndpoint :: ConnectionPool -> ThreadId -> Handler [Comment]
getCommentsEndpoint = getComments

addCommentEndpoint :: ConnectionPool -> ThreadId -> PartialComment -> Handler Comment
addCommentEndpoint = addComment

server :: ConnectionPool -> Server API
server pool = getThreadEndpoint pool
         :<|> createThreadEndpoint pool
         :<|> getCommentsEndpoint pool
         :<|> addCommentEndpoint pool
