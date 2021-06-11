{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Bot.Message.Message where

import qualified Bot.DbModels as DB
import Bot.Exception
import Bot.Message.ChatMessage (handleChatMessage, handleLawsuit)
-- import Control.Monad.Trans.Reader (ask)

-- import Control.Monad.Trans.Reader (ask)
-- import Control.Monad.Trans.Reader (ask)

-- import Control.Monad.Trans.Reader (ask)
import Bot.Message.Check
import Bot.Message.Common (MessageEnvT (runMessageEnvT), MessageHandlerEnv (MessageHandlerEnv, config, message), MyBotCommand (Lawsuit, Subscribe, Unsubscribe), command, replyBack)
import Bot.Message.DirectMessage
import Config (App, Config (Config))
import Control.Applicative (Alternative ((<|>)))
import Control.Exception (throw)
import Control.Exception.Safe (MonadCatch, throw, throwM)
import Control.Monad (guard, join, void, when)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), liftEither, mapExceptT, runExcept, runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT, runReaderT), mapReader, mapReaderT, withReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Either (fromRight, isRight)
import Data.Foldable (asum, traverse_)
import Data.Maybe (isJust)
import qualified Data.Text as T
import TgBotAPI.Common (MonadHTTP)
import TgBotAPI.Types.Chat (Chat (Chat, id))
import TgBotAPI.Types.Message (Message (Message), chat, from, text)
import TgBotAPI.Types.User (User (User, id))
import UnliftIO (MonadUnliftIO)
import Utils (liftMaybe)

getErrorText :: MessageType -> Maybe MyBotCommand -> BotException -> Maybe T.Text
getErrorText (IsChatMsg) (Just Lawsuit) exception = Just $ makeErrorReport exception
getErrorText (IsDirectMsg) (Just Subscribe) exception = Just $ makeErrorReport exception
getErrorText (IsDirectMsg) (Just Unsubscribe) exception = Just $ makeErrorReport exception
getErrorText _ _ _ = Nothing

handleMessage :: Message -> App ()
handleMessage
  message@Message
    { from = Just User {id = userTgId},
      chat = Chat {id = chatTgId},
      text
    } = do
    config <- ask

    let withCondition condition m = (do if condition then pure () else throwError NotMatched; m)

    command <- getCommand message
    messageType <- getMessageType message

    let mhe = MessageHandlerEnv {config, message, command}
    let runThis :: MessageEnvT App a -> App (Either BotException a)
        runThis = runExceptT . flip runReaderT mhe . runMessageEnvT

    let isCommand = isJust command

    res <-
      runThis $
        asum
          [ withCondition
              ( messageType == Just IsChatMsg
                  && not isCommand
              )
              handleChatMessage,
            withCondition
              ( messageType == Just IsChatMsg
                  && command == Just Lawsuit
              )
              handleLawsuit,
            withCondition
              ( messageType == Just IsDirectMsg
                  && command == Just Subscribe
              )
              handleSubscribe,
            withCondition
              ( messageType == Just IsDirectMsg
                  && command == Just Unsubscribe
              )
              handleUnsubscribe
          ]

    case res of
      Left e -> void $
        runMaybeT $ do
          messageType' <- liftMaybe messageType
          txt <- liftMaybe $ getErrorText messageType' command e
          v <- lift $ runThis $ replyBack txt
          liftIO $ putStrLn $ "replyBack" <> show v
      Right r -> pure ()
handleMessage m = liftIO $ putStr $ show m <> "\n\n"
