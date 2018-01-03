{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Control.Monad.Reader       (ReaderT)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Int                   (Int64)
import           Data.Pool                  (Pool)
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Generics               (Generic)
import           Servant

newtype Email = Email Text deriving (Show, Read, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)
newtype ThreadId = ThreadId Integer deriving (Show, Read, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

data Thread = Thread
  { id      :: Integer
  , title   :: Maybe Text
  , created :: UTCTime
  } deriving (Show, Read, Generic)

instance FromJSON Thread
instance ToJSON Thread

data PartialThread = PartialThread
  { title :: Maybe Text
  } deriving (Show, Read, Generic)

instance FromJSON PartialThread
instance ToJSON PartialThread

data Comment = Comment
  { id       :: Int64
  , created  :: UTCTime
  , text     :: Text
  , userName :: Text
  , email    :: Email
  } deriving (Show, Read, Generic)

instance FromJSON Comment
instance ToJSON Comment

data PartialComment = PartialComment
  { userName :: Text
  , email    :: Email
  , title    :: Maybe Text
  , body     :: Text
  , parentId :: Maybe Int64
} deriving (Show, Read, Generic)

instance FromJSON PartialComment
instance ToJSON PartialComment

newtype Env = Env
  { pool :: Pool Connection
  }

type AppM = ReaderT Env Handler
