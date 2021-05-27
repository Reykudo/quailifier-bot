{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Bot.DbModels where

import Bot.Models
import Config (Config, configPool)
import Control.Exception.Safe
import Control.Monad.Reader (MonadIO, MonadReader, MonadTrans (lift), ReaderT, asks, liftIO)
import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Data.Aeson (ToJSON, Value (Bool))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time
import Database.Persist (Entity (Entity), PersistEntity (Unique))
import Database.Persist.Sql (PersistFieldSql, SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import Say
import UnliftIO (MonadUnliftIO)

share
  [ mkPersist sqlSettings,
    mkMigrate "migrateAll"
  ]
  [persistLowerCase|
    User json
        tgId Int64
        subscribed Bool
        UniqueUserTgId tgId
        deriving Show Eq 
    Chat json
        tgId Int64

        UniqueChatTgId tgId
        deriving Show Eq 
    Rating json
        count Int
        user UserId
        chat ChatId
        
        UniqueRating user chat
        deriving Show Eq 

    Decision json
        initUser UserId
        initDate UTCTime
        chat ChatId
        targetUser UserId
        status DecisionStatus
        deriving Show Eq 
    |]

doMigrations :: SqlPersistT IO ()
doMigrations = do
  liftIO $ say "in doMigrations, running?"
  runMigration migrateAll
  liftIO $ say "already run"

runDb :: (MonadReader Config m, MonadIO m, MonadUnliftIO m) => SqlPersistT m b -> m b
runDb query = do
  pool <- asks configPool
  runSqlPool query pool

runDb2 :: (MonadIO m, MonadUnliftIO m) => (SqlPersistT m) b -> ReaderT Config m b
runDb2 query = do
  pool <- asks configPool
  lift $ runSqlPool query pool
