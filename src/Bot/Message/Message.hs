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

import Bot.Exception
import Bot.Message.ChatMessage (handleChatMessage)
import Bot.Message.Common (MessageEnvT (runMessageEnvT), MessageHandlerEnv (MessageHandlerEnv, config, message), replyBack, runMessageHandlerReader)
import Bot.Message.DirectMessage (handleDirectMessage)
import Bot.Models (Chat (chatTgId), User (userTgId))
import Config (Config (Config))
import Control.Applicative (Alternative ((<|>)))
import Control.Exception (throw)
import Control.Exception.Safe (throwM)
import Control.Monad (guard, join, when)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), mapExceptT, runExcept, runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT, runReaderT), mapReader, mapReaderT, withReaderT)
-- import Control.Monad.Trans.Reader (ask)

import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Foldable (asum, traverse_)
import qualified Data.Text as T
import qualified Web.Telegram.Types as TT

liftMaybe :: Applicative m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . pure

runReaderTInMonadReader :: (MonadReader r m) => m b -> ReaderT r m b
runReaderTInMonadReader = lift

handleMessage :: (MonadLogger m, MonadIO m, MonadReader Config m) => TT.Message -> m ()
handleMessage
  message@TT.Msg
    { TT.metadata =
        TT.MMetadata
          { TT.from = Just TT.User {TT.userId = userTgId},
            TT.chat = TT.Chat {TT.chatId = chatTgId}
          },
      TT.content = TT.TextM {TT.text = text}
    } = do
    config <- ask

    let withCondition condition m = (do if condition then pure () else throwError NotMatched; m)
    let isChatMessage = chatTgId < 0
    let isDirectMessage = chatTgId == userTgId
    let runIn m = runExceptT (runReaderT (runMessageEnvT m) (MessageHandlerEnv {config, message}))
    res <- runIn $ asum [withCondition isChatMessage handleChatMessage, withCondition isDirectMessage handleDirectMessage]
    liftIO $ print res
    if isDirectMessage
      then case res of
        Left e -> runIn $ replyBack $ makeErrorReport e
        Right r -> pure $ pure ()
      else pure $ pure ()
    pure ()
handleMessage m = liftIO $ putStr $ show m <> "\n\n"
