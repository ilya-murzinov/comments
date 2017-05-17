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

type API = "api" :> "comments" :>
  (
       GetCommentsEndpoint
  :<|> PostCommentEndpoint
  )

api :: Proxy API
api = Proxy

getCommentsEndpoint :: ConnectionPool -> ThreadId -> Handler [Comment]
getCommentsEndpoint = getComments

addCommentEndpoint :: ConnectionPool -> ThreadId -> PartialComment -> Handler Comment
addCommentEndpoint = addComment

server :: ConnectionPool -> Server API
server pool = getCommentsEndpoint pool :<|> addCommentEndpoint pool
