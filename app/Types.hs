{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Data.Aeson
import           Data.Text
import           Database.Persist.Sql (PersistField, PersistFieldSql)
import           Servant

newtype Email = Email Text deriving (Show, Read, FromJSON, ToJSON, PersistField, PersistFieldSql, FromHttpApiData, ToHttpApiData)
