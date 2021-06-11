{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Message.Check where

import Bot.Client (runMethodWithCache)
import Bot.Exception (BotException (NetwortError))
import Bot.Message.Common (MessageEnvT, MessageHandlerEnv (MessageHandlerEnv, config, message), MyBotCommand, getMyBotCommand, skipMHE)
import Config (App, Config (Config, configToken), GlobalCaches (getMyCommandsCache), getMethodConfiguration)
import Control.Monad (guard)
import Control.Monad.Cont (MonadTrans (lift))
import Control.Monad.Except (ExceptT, MonadError, mapExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), asks)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Text (isPrefixOf)
import Data.Time (getCurrentTime)
import qualified Network.HTTP.Client as HS
import Network.HTTP.Client.Internal (HttpException, Response (Response, responseBody))
import TgBotAPI.Common (MonadHTTP, runWithConfiguration)
import TgBotAPI.Operations.PostGetMyCommands (PostGetMyCommandsResponse (PostGetMyCommandsResponse200), PostGetMyCommandsResponseBody200 (PostGetMyCommandsResponseBody200), postGetMyCommands, result)
import TgBotAPI.Types.BotCommand (BotCommand (BotCommand), command)
import TgBotAPI.Types.Chat (Chat (..), id)
import TgBotAPI.Types.Message (Message (Message), chat, entities, from, text)
import TgBotAPI.Types.MessageEntity (MessageEntity (MessageEntity), Type (TypeEnumBotCommand), type')
import TgBotAPI.Types.User (User (User, id))

getCommand :: (MonadIO m, MonadReader Config m, MonadError HttpException m) => Message -> m (Maybe MyBotCommand)
getCommand message = do
  Config {configToken} <- ask
  mc <- getMethodConfiguration
  --   case  msg
  runMaybeT $ do
    Message {text = Just text, entities} <- pure message
    Just entities' <- pure entities
    guard $ any (\case MessageEntity {type' = TypeEnumBotCommand} -> True; _ -> False) entities'
    -- fromTime <- liftIO getCurrentTime
    Response
      { responseBody = PostGetMyCommandsResponse200 PostGetMyCommandsResponseBody200 {result = botCommands}
      } <-
      lift $ runMethodWithCache getMyCommandsCache () $ const postGetMyCommands

    Just BotCommand {command} <- pure $ find (\case BotCommand {command} -> isPrefixOf ("/" <> command) text) botCommands
    liftIO $ print command
    pure $ getMyBotCommand command

data MessageType = IsChatMsg | IsDirectMsg deriving (Eq)

-- isDirectMessage :: (MonadIO m) => m Bool
-- getMessageType :: Monad f => Message -> f MessageType
getMessageType :: Monad m => Message -> m (Maybe MessageType)
getMessageType message = runMaybeT $ do
  let Chat {id = chatId} = chat message
  User {id = userId} <- MaybeT . pure $ from message
  MaybeT $ pure $ if chatId < 0 then Just IsChatMsg else (if chatId == userId then Just IsDirectMsg else Nothing)
