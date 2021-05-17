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

import Bot.Client (sendMessage)
import qualified Bot.DbModels as BM
import Bot.Exception
import Config (Config (Config, configToken))
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

data MessageHandlerEnv = MessageHandlerEnv {config :: Config, message :: TG.Message}

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
  TG.Msg {metadata = TG.MMetadata {messageId, from = Just TG.User {userId}, chat = TG.Chat {chatId}}} <- asks message
  let sendBack = \text ->
        liftIO $
          sendMessage
            configToken
            ( TGS.SMsg
                { chatId = ChatId chatId,
                  text = text,
                  disableWebPagePreview = Nothing,
                  parseMode = Nothing,
                  disableNotification = Nothing,
                  replyToMessageId = Just $ fromIntegral messageId,
                  -- replyToMessageId = Nothing,
                  replyMarkup = Nothing
                }
            )
  sendBack e
  pure ()
