{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Bot.Message.ChatMessage where

import qualified Bot.DbModels as DB
import Bot.Exception (BotExceptT, BotException)
import Bot.Message.Common
import Bot.Models (DecisionStatus (Process))
import Config (AppT (AppT), Config)
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except (MonadError)
import Control.Monad.Logger (MonadLogger, logDebugNS)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import TgBotAPI.Types.Chat (Chat (Chat, id))
import TgBotAPI.Types.Message (Message (Message), chat, from, replyToMessage, text)
import TgBotAPI.Types.User (User (User, id))

handleChatMessage :: (MonadLogger m, MonadIO m, MonadReader MessageHandlerEnv m, MonadFail m, MonadError BotException m) => m ()
handleChatMessage = do
  -- increment "createUser"
  --   logDebugNS "web" "creating a user"
  Message {chat = Chat {id = chatTgId}, from = Just User {id = userTgId}, text = Just messageText} <- asks message

  let count = T.length messageText
  runDbInMsgEnv
    ( do
        user <- upsertBy (DB.UniqueUserTgId userTgId) (DB.User userTgId False) [DB.UserTgId =. userTgId]
        chat <- upsertBy (DB.UniqueChatTgId chatTgId) (DB.Chat chatTgId) [DB.ChatTgId =. chatTgId]
        ratingEntity <-
          upsertBy
            ( DB.UniqueRating
                (entityKey user)
                (entityKey chat)
            )
            ( DB.Rating
                { DB.ratingCount = count,
                  DB.ratingUser = entityKey user,
                  DB.ratingChat = entityKey chat
                }
            )
            [DB.RatingCount +=. count]

        pure ()
    )

  pure ()

handleChatMessageCommand :: (MonadLogger m, MonadIO m, MonadReader MessageHandlerEnv m, MonadFail m, MonadError BotException m) => m ()
handleChatMessageCommand = do
  Message {chat = Chat {id = chatTgId}, from = Just User {id = userTgId}, text = Just messageText, replyToMessage = Just Message {from = Just User {id = targetUserTgId}}} <- asks message

  let count = T.length messageText
  decisionInitDate <- liftIO getCurrentTime
  e <- runDbInMsgEnv $ do
    Just initUser <- getBy $ DB.UniqueUserTgId userTgId
    Just targetUser <- getBy $ DB.UniqueUserTgId targetUserTgId
    Just chat <- getBy $ DB.UniqueChatTgId chatTgId
    let entity =
          DB.Decision
            { decisionInitUser = entityKey initUser,
              decisionTargetUser = entityKey targetUser,
              decisionInitDate,
              decisionChat = entityKey chat,
              decisionStatus = Process
            }
    liftIO $ print entity
    insert entity

  replyBack $ "Success" <> T.pack (show e)
