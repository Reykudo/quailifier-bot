{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Bot.Message.Message where

import qualified Bot.DbModels as DB
import Bot.Exception
import Bot.Message.ChatMessage (handleChatMessage, handleChatMessageCommand)
-- import Control.Monad.Trans.Reader (ask)

-- import Control.Monad.Trans.Reader (ask)
-- import Control.Monad.Trans.Reader (ask)

-- import Control.Monad.Trans.Reader (ask)
import Bot.Message.Check (isChatMessage, isCommand, isDirectMessage)
import Bot.Message.Common (MessageEnvT (runMessageEnvT), MessageHandlerEnv (MessageHandlerEnv, config, message), replyBack, runMessageHandlerReader)
import Bot.Message.DirectMessage (handleDirectMessage)
import Config (Config (Config))
import Control.Applicative (Alternative ((<|>)))
import Control.Exception (throw)
import Control.Exception.Safe (throw, throwM)
import Control.Monad (guard, join, when)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), mapExceptT, runExcept, runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT, runReaderT), mapReader, mapReaderT, withReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Foldable (asum, traverse_)
import qualified Data.Text as T
import TgBotAPI.Common (MonadHTTP)
import TgBotAPI.Types.Chat (Chat (Chat, id))
import TgBotAPI.Types.Message (Message (Message), chat, from, text)
import TgBotAPI.Types.User (User (User, id))

liftMaybe :: Applicative m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . pure

handleMessage :: (MonadLogger m, MonadIO m, MonadReader Config m, MonadHTTP m) => Message -> m ()
handleMessage
  message@Message
    { from = Just User {id = userTgId},
      chat = Chat {id = chatTgId},
      text
    } = do
    config <- ask

    let withCondition condition m = (do if condition then pure () else throwError NotMatched; m)

    flip runReaderT (MessageHandlerEnv {config, message}) $ do
      isCommand' <- isCommand
      isDirectMessage' <- isDirectMessage
      isChatMessage' <- isChatMessage

      liftIO $ putStrLn $ "isCommand" <> show isCommand'
      res <-
        runExceptT $
          asum
            [ withCondition (isChatMessage' && isCommand') handleChatMessageCommand,
              withCondition (isChatMessage' && not isCommand') handleChatMessage,
              withCondition (isDirectMessage' && isCommand') handleDirectMessage
            ]
      liftIO $ print res
      a <-
        ( if isDirectMessage' || isCommand'
            then case res of
              Left e -> runExceptT $ replyBack $ makeErrorReport e
              Right r -> pure $ pure ()
            else pure $ pure ()
          )
      liftIO $ putStrLn $ "replyBack" <> show a
      pure ()
    pure ()
handleMessage m = liftIO $ putStr $ show m <> "\n\n"
