{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Message.Check where

import Bot.Client (getMyCommands)
import Bot.Message.Common (MessageHandlerEnv (MessageHandlerEnv, config, message))
import Config (Config (Config, configToken))
import Control.Monad (guard)
import Control.Monad.Cont (MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Maybe (fromMaybe)
import Data.Text (isPrefixOf)
import qualified Web.Telegram.Types as TG
import Web.Telegram.Types.Update (ReqResult (Ok))

isCommand :: (MonadIO m, MonadReader MessageHandlerEnv m) => m Bool
isCommand = do
  TG.Msg {content} <- asks message
  Config {configToken} <- asks config
  --   case  msg
  case content of
    TG.TextM {text, entities} -> (fromMaybe False <$>) $
      runMaybeT $ do
        Just entities' <- pure entities
        guard $ any (\case TG.MessageEntity {entityType = TG.BotCommand} -> True; _ -> False) entities'
        Ok botCommands <- getMyCommands configToken
        liftIO $ print botCommands
        guard $ any (\case TG.BC {command} -> isPrefixOf ("/" <> command) text) botCommands
        pure True
    _ -> pure False

isDirectMessage :: (MonadIO m, MonadReader MessageHandlerEnv m) => m Bool
isDirectMessage = do
  msg <- asks message

  (fromMaybe False <$>) $
    runMaybeT $ do
      let chatId = TG.chatId $ TG.chat $ TG.metadata msg
      TG.User {userId} <- MaybeT . pure $ TG.from (TG.metadata msg)
      pure $ chatId == userId

isChatMessage :: (MonadIO m, MonadReader MessageHandlerEnv m) => m Bool
isChatMessage = do
  msg <- asks message
  let chatId = TG.chatId $ TG.chat $ TG.metadata msg
  pure $ chatId < 0
