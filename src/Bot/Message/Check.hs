{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Message.Check where

import Bot.Client (runMethodWithCache)
import Bot.Exception (BotException)
import Bot.Message.Common (MessageEnvT, MessageHandlerEnv (MessageHandlerEnv, config, message), skipMHE)
import Config (App, Config (Config, configToken), GlobalCaches (getMyCommandsCache), getMethodConfiguration)
import Control.Monad (guard)
import Control.Monad.Cont (MonadTrans (lift))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Maybe (fromMaybe)
import Data.Text (isPrefixOf)
import Data.Time (getCurrentTime)
import Network.HTTP.Client.Internal (HttpException, Response (Response, responseBody))
import TgBotAPI.Common (MonadHTTP, runWithConfiguration)
import TgBotAPI.Operations.PostGetMyCommands (PostGetMyCommandsResponse (PostGetMyCommandsResponse200), PostGetMyCommandsResponseBody200 (PostGetMyCommandsResponseBody200), postGetMyCommands, result)
import TgBotAPI.Types.BotCommand (BotCommand (BotCommand), command)
import TgBotAPI.Types.Chat (id)
import TgBotAPI.Types.Message (Message (Message), chat, entities, from, text)
import TgBotAPI.Types.MessageEntity (MessageEntity (MessageEntity), Type (TypeEnumBotCommand), type')
import TgBotAPI.Types.User (User (User, id))

isCommand :: (MonadIO m) => MessageEnvT m Bool
isCommand = do
  Config {configToken} <- asks config
  mc <- skipMHE getMethodConfiguration
  --   case  msg
  (fromMaybe False <$>) $
    runMaybeT $ do
      Message {text = Just text, entities} <- asks message
      Just entities' <- pure entities
      guard $ any (\case MessageEntity {type' = TypeEnumBotCommand} -> True; _ -> False) entities'
      -- fromTime <- liftIO getCurrentTime
      Response {responseBody = PostGetMyCommandsResponse200 PostGetMyCommandsResponseBody200 {result = botCommands}} <- skipMHE $ runMethodWithCache getMyCommandsCache () $ const postGetMyCommands
      -- toTime <- liftIO getCurrentTime
      -- liftIO $ putStrLn $ "from: " <> show fromTime <> " to " <> show toTime
      -- liftIO $ print botCommands
      guard $ any (\case BotCommand {command} -> isPrefixOf ("/" <> command) text) botCommands
      pure True

isDirectMessage :: (MonadIO m) => MessageEnvT m Bool
isDirectMessage = do
  msg <- asks message

  (fromMaybe False <$>) $
    runMaybeT $ do
      let chatId = TgBotAPI.Types.Chat.id $ chat msg
      User {id = userId} <- MaybeT . pure $ from msg
      pure $ chatId == userId

isChatMessage :: (MonadIO m) => MessageEnvT m Bool
isChatMessage = do
  msg <- asks message
  let chatId = TgBotAPI.Types.Chat.id $ chat msg
  pure $ chatId < 0
