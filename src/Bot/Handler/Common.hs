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

import qualified Bot.Models as BM
import Config (Config (Config, configToken))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Data.Text as T
import Database.Persist.Postgresql (SqlPersistT)
import TgBotAPI.Common (Configuration (Configuration, configBaseURL, configSecurityScheme), anonymousSecurityScheme)
import TgBotAPI.Operations.PostSendMessage (ChatIdVariants (ChatIdInt), PostSendMessageRequestBody (PostSendMessageRequestBody), allowSendingWithoutReply, chatId, disableNotification, disableWebPagePreview, entities, parseMode, postSendMessage, postSendMessageWithConfiguration, replyMarkup, replyToMessageId, text)

import TgBotAPI.Types.Chat (Chat (Chat, id))
import TgBotAPI.Types.Message (Message (Message), chat, from, messageId)
import TgBotAPI.Types.User (User (User, id))

data MessageHandlerEnv = MessageHandlerEnv {config :: Config, message :: Message}

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
  Message {messageId, from = Just User {id = userId}, chat = Chat {id = chatId}} <- asks message
  liftIO $
    postSendMessageWithConfiguration
      (Configuration {configBaseURL = "", configSecurityScheme = anonymousSecurityScheme})
      ( PostSendMessageRequestBody
          { chatId = ChatIdInt chatId,
            text = e,
            disableWebPagePreview = Nothing,
            parseMode = Nothing,
            disableNotification = Nothing,
            replyToMessageId = Just messageId,
            replyMarkup = Nothing,
            allowSendingWithoutReply = Nothing,
            entities = Nothing
          }
      )
  pure ()
