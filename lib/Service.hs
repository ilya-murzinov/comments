module Service where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import           Data.Time              (UTCTime, getCurrentTime)

import           Persistence
import           Types

getThread :: ThreadId -> m (Maybe Thread)
getThread (ThreadId tid) = undefined

createThread :: PartialThread -> m Thread
createThread (PartialThread title) = undefined

getComments :: ThreadId -> m [Comment]
getComments (ThreadId tid) = undefined

addComment :: ThreadId -> PartialComment -> m (Maybe Comment)
addComment (ThreadId tid) (PartialComment n e t b mP) = undefined
