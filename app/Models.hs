{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Data.Text
import           Data.Time
import           Database.Persist.Sqlite
import           Database.Persist.TH

import           Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name Text
    email Email
    deriving Show Read
Comment json
    userId UserId
    threadId ThreadId
    created UTCTime
    parentId CommentId Maybe
    title Text
    body Text
    deriving Show Read
Thread json
    title Text
    created UTCTime
    deriving Show Read
|]
