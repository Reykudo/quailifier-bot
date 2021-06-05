{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Bot.Message.Common where

-- import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))

import Bot.Client (runMethod)
import qualified Bot.DbModels as BM
import Bot.Exception
import Config (App, Config (Config, configCache, configToken))
import Control.Applicative (Alternative)
import Control.Exception.Safe (Exception, MonadCatch, throw, throwIO, try)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), MonadPlus, MonadTrans (lift), mapExceptT, runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Control.Monad.Trans.Reader hiding (ask, asks)
import Data.Bifunctor (Bifunctor (second))
import Data.String (IsString)
import qualified Data.Text as T
import Database.Persist.Postgresql (SqlPersistT)
import qualified Network.HTTP.Client as HS
import TgBotAPI.Common (Configuration (Configuration, configBaseURL, configSecurityScheme), MonadHTTP (httpBS), anonymousSecurityScheme)
import TgBotAPI.Operations.PostSendMessage (ChatIdVariants (ChatIdInt), PostSendMessageRequestBody (PostSendMessageRequestBody), PostSendMessageResponse, ReplyMarkup (..), allowSendingWithoutReply, chatId, disableNotification, disableWebPagePreview, entities, parseMode, postSendMessage, postSendMessageWithConfiguration, replyMarkup, replyToMessageId, text)
import TgBotAPI.Types.Chat (Chat (Chat, id))
import TgBotAPI.Types.Message (Message (Message), chat, from, messageId)
import TgBotAPI.Types.User (User (User, id))
import UnliftIO (MonadUnliftIO (withRunInIO), UnliftIO (unliftIO), withUnliftIO)

data MessageHandlerEnv = MessageHandlerEnv {config :: Config, message :: Message, command :: Maybe MyBotCommand}

data MyBotCommand = Start | Unknown | Unsubscribe | Subscribe | Lawsuit
  deriving (Show, Eq)

fromNetworkError :: Functor m => ExceptT HS.HttpException m b -> ExceptT BotException m b
fromNetworkError = withExceptT NetwortError

getMyBotCommand :: (Eq a, IsString a) => a -> MyBotCommand
getMyBotCommand text = case text of
  "start" -> Start
  "subscribe" -> Subscribe
  "unsubscribe" -> Unsubscribe
  "lawsuit" -> Lawsuit
  _ -> Unknown

newtype MessageEnvT m a = MessageEnvT
  {runMessageEnvT :: ReaderT MessageHandlerEnv (ExceptT BotException m) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader MessageHandlerEnv,
      MonadError BotException,
      MonadIO,
      MonadLogger,
      Alternative
    )

deriving instance MonadUnliftIO (MessageEnvT App)

instance Monad m => MonadFail (MessageEnvT m) where
  fail = throwError . RawText . T.pack

-- withConfig :: (MonadReader MessageHandlerEnv m, MonadIO m) => SqlPersistT IO b -> m b
skipMHE m = do
  cfg <- asks config
  runReaderT m cfg

-- runMessageHandlerReader :: (MonadReader Config m, MonadReader MessageHandlerEnv m) => TG.Message -> n b -> m b
-- runMessageHandlerReader :: MonadReader Config m => TG.Message -> ReaderT MessageHandlerEnv m b -> m b
-- runMessageHandlerReader message target = do
--   config <- ask
--   target `runReaderT` MessageHandlerEnv {message, config}

runDbInMsgEnv :: (MonadIO m, MonadUnliftIO m, MonadReader MessageHandlerEnv m) => (SqlPersistT m) b -> m b
runDbInMsgEnv = skipMHE . BM.runDb2

instance (Monad m, MonadHTTP m) => MonadHTTP (ExceptT BotException (ReaderT MessageHandlerEnv m)) where
  httpBS r = lift $ lift (httpBS r)

replyBack :: (Monad m, MonadIO m) => T.Text -> MessageEnvT m (HS.Response PostSendMessageResponse)
replyBack e = do
  configToken <- asks $ configToken . config
  Message {messageId, from = Just User {id = userId}, chat = Chat {id = chatId}} <- asks message
  a <-
    runExceptT $fromNetworkError $
      skipMHE $
        runMethod
          ( postSendMessage
              ( PostSendMessageRequestBody
                  { chatId = ChatIdInt chatId,
                    text = e,
                    disableWebPagePreview = Nothing,
                    parseMode = Just "HTML",
                    disableNotification = Nothing,
                    replyToMessageId = Just messageId,
                    replyMarkup =
                      Just $
                        ReplyMarkup
                          { forceReply = Just False,
                            inlineKeyboard = Just [],
                            keyboard = Just [],
                            oneTimeKeyboard = Just False,
                            removeKeyboard = Just False,
                            resizeKeyboard = Just False,
                            selective = Just False
                          },
                    allowSendingWithoutReply = Nothing,
                    entities = Nothing
                  }
              )
          )
  either throwError pure a

-- pure $ either throwError Prelude.id a
