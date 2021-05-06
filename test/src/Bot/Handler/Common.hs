{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Bot.Handler.Common where

import Bot.Client (sendMessage)
import qualified Bot.Models as BM
import Config (Config (Config, configToken))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Data.Text as T
import Database.Persist.Postgresql (SqlPersistT)
import Web.Telegram.API (ChatId (ChatId))
import qualified Web.Telegram.API (ChatId)
import qualified Web.Telegram.API.Sending.Data as TGS
import qualified Web.Telegram.Types as TG

data MessageHandlerEnv = MessageHandlerEnv {config :: Config, message :: TG.Message}

type MessageHandlerReader = MonadReader MessageHandlerEnv

-- withConfig :: (MonadReader MessageHandlerEnv m, MonadIO m) => SqlPersistT IO b -> m b
skipMHE m = do
  cfg <- asks config
  runReaderT m cfg

-- runMessageHandlerReader :: (MonadReader Config m, MonadReader MessageHandlerEnv m) => TG.Message -> n b -> m b
-- runMessageHandlerReader :: MonadReader Config m => TG.Message -> ReaderT MessageHandlerEnv m b -> m b
runMessageHandlerReader message target = do
  config <- ask
  target `runReaderT` MessageHandlerEnv {message, config}

runDbMHE q = skipMHE $ BM.runDb q

replyBack :: (MessageHandlerReader m, MonadIO m) => T.Text -> MaybeT m ()
replyBack e = do
  configToken <- asks $ configToken . config
  TG.Msg {metadata = TG.MMetadata {messageId, from = Just TG.User {userId}}} <- asks message
  let sendBack = \text ->
        liftIO $
          sendMessage
            configToken
            ( TGS.SMsg
                { chatId = ChatId userId,
                  text = text,
                  disableWebPagePreview = Nothing,
                  parseMode = Nothing,
                  disableNotification = Nothing,
                  replyToMessageId = Just $ fromIntegral messageId,
                  replyMarkup = Nothing
                }
            )
  sendBack e
  pure ()
