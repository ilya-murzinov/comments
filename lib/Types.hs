{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types where

import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Aeson.TH        (defaultOptions, deriveJSON,
                                       fieldLabelModifier)
import           Data.Char            (toLower)
import           Data.Int             (Int64)
import           Data.Text            (Text)
import           Data.Time            (UTCTime)
import           Database.Persist.Sql (PersistField, PersistFieldSql)
import           GHC.Generics         (Generic)
import           Servant

newtype Email = Email Text deriving (Show, Read, FromJSON, ToJSON, PersistField, PersistFieldSql, FromHttpApiData, ToHttpApiData)
newtype ThreadId = ThreadId Int64 deriving (Show, Read, FromJSON, ToJSON, PersistField, PersistFieldSql, FromHttpApiData, ToHttpApiData)
newtype ThreadIdentifier = ThreadIdentifier Text deriving (Show, Read, FromJSON, ToJSON, PersistField, PersistFieldSql, FromHttpApiData, ToHttpApiData)

data Thread = Thread
  { threadId         :: Int64
  , threadIdentifier :: Text
  , threadTitle      :: Maybe Text
  , threadCreated    :: UTCTime
  } deriving (Show, Read)

$(deriveJSON defaultOptions{ fieldLabelModifier = map toLower . drop 6 } ''Thread)

data PartialThread = PartialThread
  { partialThreadIdentifier :: Text
  , partialThreadTitle      :: Maybe Text
  } deriving (Show, Read)

$(deriveJSON defaultOptions{ fieldLabelModifier = map toLower . drop 13 } ''PartialThread)

data Comment = Comment
  { commentId       :: Int64
  , commentCreated  :: UTCTime
  , commentText     :: Text
  , commentUserName :: Text
  , commentEmail    :: Email
  } deriving (Show, Read, Generic)

$(deriveJSON defaultOptions{ fieldLabelModifier = map toLower . drop 7 } ''Comment)

data PartialComment = PartialComment
  { userName  :: Text
  , userEmail :: Email
  , title     :: Maybe Text
  , body      :: Text
  , parentId  :: Maybe Int64
} deriving (Show, Read, Generic)

instance FromJSON PartialComment
instance ToJSON PartialComment
