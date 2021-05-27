{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Bot.UpdateHandler where

import Bot.Message.Message (handleMessage)
import qualified Bot.Models as MDLS
import Config (App, AppT (AppT), Config)
import Control.Exception.Safe
import Control.Monad (void)
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger, logDebugNS)
import Control.Monad.RWS (MonadReader)
import Control.Monad.Reader
  ( ReaderT (ReaderT, runReaderT),
    ask,
  )
import Control.Monad.Trans (lift)
import Data.Int (Int64)
import qualified Data.Text as T
import Database.Persist ((=.))
import Database.Persist.Postgresql (Entity (entityKey), PersistUniqueWrite (upsertBy), (+=.))
import TgBotAPI.Common
import TgBotAPI.Types.Message (Message (Message))
import TgBotAPI.Types.Update (Update (Update), message)

runReaderTIn :: MonadReader r m => ReaderT r m b -> m b
runReaderTIn m = ask >>= runReaderT m

updateHandler :: Update -> App ()
updateHandler Update {message = Just message} = handleMessage message
updateHandler update = liftIO $ print update
