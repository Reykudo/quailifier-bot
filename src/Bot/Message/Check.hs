{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Message.Check where

import Bot.Message.Common (MessageHandlerEnv (MessageHandlerEnv, config, message), skipMHE)
import Config (Config (Config, configToken), getMethodConfiguration, runMethod)
import Control.Monad (guard)
import Control.Monad.Cont (MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Maybe (fromMaybe)
import Data.Text (isPrefixOf)
import Network.HTTP.Client.Internal (Response (Response, responseBody))
import TgBotAPI.Common (MonadHTTP, runWithConfiguration)
import TgBotAPI.Operations.PostGetMyCommands (PostGetMyCommandsResponse (PostGetMyCommandsResponse200), PostGetMyCommandsResponseBody200 (PostGetMyCommandsResponseBody200), postGetMyCommands, result)
import TgBotAPI.Types.BotCommand (BotCommand (BotCommand), command)
import TgBotAPI.Types.Chat (id)
import TgBotAPI.Types.Message (Message (Message), chat, entities, from, text)
import TgBotAPI.Types.MessageEntity (MessageEntity (MessageEntity), Type (TypeEnumBotCommand), type')
import TgBotAPI.Types.User (User (User, id))

isCommand :: (MonadIO m, MonadReader MessageHandlerEnv m, MonadHTTP m) => m Bool
isCommand = do
  Config {configToken} <- asks config
  mc <- skipMHE getMethodConfiguration
  --   case  msg
  (fromMaybe False <$>) $
    runMaybeT $ do
      Message {text = Just text, entities} <- asks message
      Just entities' <- pure entities
      guard $ any (\case MessageEntity {type' = TypeEnumBotCommand} -> True; _ -> False) entities'
      Response {responseBody = PostGetMyCommandsResponse200 PostGetMyCommandsResponseBody200 {result = botCommands}} <- lift $ runWithConfiguration mc postGetMyCommands
      liftIO $ print botCommands
      guard $ any (\case BotCommand {command} -> isPrefixOf ("/" <> command) text) botCommands
      pure True

isDirectMessage :: (MonadIO m, MonadReader MessageHandlerEnv m) => m Bool
isDirectMessage = do
  msg <- asks message

  (fromMaybe False <$>) $
    runMaybeT $ do
      let chatId = TgBotAPI.Types.Chat.id $ chat msg
      User {id = userId} <- MaybeT . pure $ from msg
      pure $ chatId == userId

isChatMessage :: (MonadIO m, MonadReader MessageHandlerEnv m) => m Bool
isChatMessage = do
  msg <- asks message
  let chatId = TgBotAPI.Types.Chat.id $ chat msg
  pure $ chatId < 0
