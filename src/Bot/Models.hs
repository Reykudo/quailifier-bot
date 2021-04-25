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

module Bot.Models where

import Config (Config, configPool)
import Control.Exception.Safe
import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import Say
import Servant.API.Generic (Generic)

share
  [ mkPersist sqlSettings,
    mkMigrate "migrateAll"
  ]
  [persistLowerCase|
    User json
        tgId Int

        UniqueUserTgId tgId
        deriving Show Eq 
    Chat json
        tgId Int

        UniqueChatTgId tgId
        deriving Show Eq 
    Rating json
        count Int
        user UserId
        chat ChatId
        
        UniqueRating user chat
        deriving Show Eq 
    |]

doMigrations :: SqlPersistT IO ()
doMigrations = do
  liftIO $ say "in doMigrations, running?"
  runMigration migrateAll
  liftIO $ say "already run"

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool
