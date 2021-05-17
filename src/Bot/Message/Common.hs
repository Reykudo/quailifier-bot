{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Bot.Message.Common where

import qualified Bot.DbModels as BM
import Bot.Exception
import Config (Config (Config, configToken), runMethod)
-- import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))

import Control.Applicative (Alternative)
import Control.Monad.Except (ExceptT (ExceptT), MonadError, MonadPlus)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Control.Monad.Trans.Reader hiding (ask, asks)
import qualified Data.Text as T
import Database.Persist.Postgresql (SqlPersistT)
import TgBotAPI.Common (Configuration (Configuration, configBaseURL, configSecurityScheme), anonymousSecurityScheme)
import TgBotAPI.Operations.PostSendMessage (ChatIdVariants (ChatIdInt), PostSendMessageRequestBody (PostSendMessageRequestBody), allowSendingWithoutReply, chatId, disableNotification, disableWebPagePreview, entities, parseMode, postSendMessage, postSendMessageWithConfiguration, replyMarkup, replyToMessageId, text)
import TgBotAPI.Types.Chat (Chat (Chat, id))
import TgBotAPI.Types.Message (Message (Message), chat, from, messageId)
import TgBotAPI.Types.User (User (User, id))

data MessageHandlerEnv = MessageHandlerEnv {config :: Config, message :: Message}

newtype MessageEnvT m a = MessageEnvT
  {runMessageEnvT :: ReaderT MessageHandlerEnv (ExceptT BotException m) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader MessageHandlerEnv,
      MonadError BotException,
      MonadIO,
      MonadFail,
      MonadLogger,
      Alternative
    )

-- deriving newtype instance (Monad m)=> Alternative (MessageEnvT m)

-- instance MonadFail MessageEnvT where
--   fail =  ExceptT . pure . Left . RawText . T.pack
-- withConfig :: (MonadReader MessageHandlerEnv m, MonadIO m) => SqlPersistT IO b -> m b
skipMHE m = do
  cfg <- asks config
  runReaderT m cfg

-- runMessageHandlerReader :: (MonadReader Config m, MonadReader MessageHandlerEnv m) => TG.Message -> n b -> m b
-- runMessageHandlerReader :: MonadReader Config m => TG.Message -> ReaderT MessageHandlerEnv m b -> m b
runMessageHandlerReader message target = do
  config <- ask
  target `runReaderT` MessageHandlerEnv {message, config}

runDbInMsgEnv q = skipMHE $ BM.runDb q

replyBack :: (MonadIO m, MonadReader MessageHandlerEnv m, MonadFail m, MonadError BotException m) => T.Text -> m ()
replyBack e = do
  configToken <- asks $ configToken . config
  Message {messageId, from = Just User {id = userId}, chat = Chat {id = chatId}} <- asks message
  skipMHE $
    runMethod
      postSendMessageWithConfiguration
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
