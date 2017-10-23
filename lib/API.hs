{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Monoid                ((<>))
import           Servant

import           Service
import           Types

type TID = Capture "threadId" ThreadId
type GetCommentsEndpoint = TID :> Get '[JSON] [Comment]
type PostCommentEndpoint = TID :> ReqBody '[JSON] PartialComment :>
                                  PostCreated '[JSON] Comment
type GetThreadEndpoint = Capture "threadId" ThreadId :> Get '[JSON] Thread
type CreateThreadEndpoint = ReqBody '[JSON] PartialThread :> PostCreated '[JSON] Thread

type API = "api" :>
  (    "threads" :> GetThreadEndpoint
  :<|> "threads" :> CreateThreadEndpoint
  :<|> "comments" :> GetCommentsEndpoint
  :<|> "comments" :> PostCommentEndpoint
  )

api :: Proxy API
api = Proxy

threadNotExists :: String -> ServantErr
threadNotExists tid = err404 {errBody = "Thread " <> pack tid <> " does not exist"}

getThreadEndpoint :: ThreadId -> Handler Thread
getThreadEndpoint tid@(ThreadId i) = do
  mThread <- getThread tid
  case mThread of
    Just thread -> return thread
    Nothing     -> throwError $ threadNotExists $ show i

createThreadEndpoint :: PartialThread -> Handler Thread
createThreadEndpoint = createThread

getCommentsEndpoint :: ThreadId -> Handler [Comment]
getCommentsEndpoint = getComments

addCommentEndpoint :: ThreadId -> PartialComment -> Handler Comment
addCommentEndpoint tid@(ThreadId i) c = do
  mComment <- addComment tid c
  case mComment of
    Just comment -> return comment
    Nothing      -> throwError $ threadNotExists $ show i

server :: Server API
server = getThreadEndpoint
         :<|> createThreadEndpoint
         :<|> getCommentsEndpoint
         :<|> addCommentEndpoint
