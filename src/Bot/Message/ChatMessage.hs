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
import Bot.Exception
import Bot.Message.Common
import Bot.Models (DecisionStatus (Process))
import Config (App, AppT (AppT), Config)
import Control.Monad.Cont (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.Except (MonadError (catchError, throwError), mapExceptT, runExceptT)
import Control.Monad.Logger (MonadLogger, logDebugNS)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import TgBotAPI.Common
import TgBotAPI.Types.Chat (Chat (Chat, id))
import TgBotAPI.Types.Message (Message (Message), chat, from, replyToMessage, text)
import TgBotAPI.Types.User (User (User, id))
import UnliftIO (MonadUnliftIO)

handleChatMessage :: (Monad m, MonadIO m, MonadUnliftIO (MessageEnvT m)) => MessageEnvT m ()
handleChatMessage = do
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

handleChatMessageCommand :: (Monad m, MonadIO m, MonadUnliftIO (MessageEnvT m)) => MessageEnvT m ()
handleChatMessageCommand = do
  m <- asks message

  Message {chat = Chat {id = chatTgId}, from = Just User {id = userTgId}, text = Just messageText, replyToMessage = Just Message {from = Just User {id = targetUserTgId}}} <- pure m

  let count = T.length messageText
  decisionInitDate <- liftIO getCurrentTime
  e <- runDbInMsgEnv $ do
    maybeInitUser <- getBy $ DB.UniqueUserTgId userTgId
    maybeTargetUser <- getBy $ DB.UniqueUserTgId targetUserTgId
    maybeChat <- getBy $ DB.UniqueChatTgId chatTgId

    chat <- maybe (throwError ChatNotFound) pure maybeChat
    initUser <- maybe (throwError (UserNotFound Suitor)) pure maybeInitUser
    targetUser <- maybe (throwError (UserNotFound Defendant)) pure maybeTargetUser

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
  pure ()

-- a =
--   Message
--     { animation = Nothing,
--       audio = Nothing,
--       authorSignature = Nothing,
--       caption = Nothing,
--       captionEntities = Nothing,
--       channelChatCreated = Nothing,
--       chat =
--         Chat
--           { bio = Nothing,
--             canSetStickerSet = Nothing,
--             description = Nothing,
--             firstName = Nothing,
--             id = -549343125,
--             inviteLink = Nothing,
--             lastName = Nothing,
--             linkedChatId = Nothing,
--             location = Nothing,
--             messageAutoDeleteTime = Nothing,
--             permissions = Nothing,
--             photo = Nothing,
--             pinnedMessage = Nothing,
--             slowModeDelay = Nothing,
--             stickerSetName = Nothing,
--             title = Just "test2",
--             type' = TypeEnumGroup,
--             username = Nothing
--           },
--       connectedWebsite = Nothing,
--       contact = Nothing,
--       date = 1622147130,
--       deleteChatPhoto = Nothing,
--       dice = Nothing,
--       document = Nothing,
--       editDate = Nothing,
--       entities = Just [MessageEntity {language = Nothing, length = 15, offset = 0, type' = TypeEnumBotCommand, url = Nothing, user = Nothing}],
--       forwardDate = Nothing,
--       forwardFrom = Nothing,
--       forwardFromChat = Nothing,
--       forwardFromMessageId = Nothing,
--       forwardSenderName = Nothing,
--       forwardSignature = Nothing,
--       from =
--         Just
--           ( User
--               { canJoinGroups = Nothing,
--                 canReadAllGroupMessages = Nothing,
--                 firstName = "R\926YK",
--                 id = 877072184,
--                 isBot = False,
--                 languageCode = Just "ru",
--                 lastName = Nothing,
--                 supportsInlineQueries = Nothing,
--                 username = Just "einlied"
--               }
--           ),
--       game = Nothing,
--       groupChatCreated = Nothing,
--       invoice = Nothing,
--       leftChatMember = Nothing,
--       location = Nothing,
--       mediaGroupId = Nothing,
--       messageAutoDeleteTimerChanged = Nothing,
--       messageId = 841,
--       migrateFromChatId = Nothing,
--       migrateToChatId = Nothing,
--       newChatMembers = Nothing,
--       newChatPhoto = Nothing,
--       newChatTitle = Nothing,
--       passportData = Nothing,
--       photo = Nothing,
--       pinnedMessage = Nothing,
--       poll = Nothing,
--       proximityAlertTriggered = Nothing,
--       replyMarkup = Nothing,
--       replyToMessage =
--         Just
--           ( Message
--               { animation = Nothing,
--                 audio = Nothing,
--                 authorSignature = Nothing,
--                 caption = Nothing,
--                 captionEntities = Nothing,
--                 channelChatCreated = Nothing,
--                 chat =
--                   Chat
--                     { bio = Nothing,
--                       canSetStickerSet = Nothing,
--                       description = Nothing,
--                       firstName = Nothing,
--                       id = -549343125,
--                       inviteLink = Nothing,
--                       lastName = Nothing,
--                       linkedChatId = Nothing,
--                       location = Nothing,
--                       messageAutoDeleteTime = Nothing,
--                       permissions = Nothing,
--                       photo = Nothing,
--                       pinnedMessage = Nothing,
--                       slowModeDelay = Nothing,
--                       stickerSetName = Nothing,
--                       title = Just "test2",
--                       type' = TypeEnumGroup,
--                       username = Nothing
--                     },
--                 connectedWebsite = Nothing,
--                 contact = Nothing,
--                 date = 1622147040,
--                 deleteChatPhoto = Nothing,
--                 dice = Nothing,
--                 document = Nothing,
--                 editDate = Nothing,
--                 entities = Just [MessageEntity {language = Nothing, length = 15, offset = 0, type' = TypeEnumBotCommand, url = Nothing, user = Nothing}],
--                 forwardDate = Nothing,
--                 forwardFrom = Nothing,
--                 forwardFromChat = Nothing,
--                 forwardFromMessageId = Nothing,
--                 forwardSenderName = Nothing,
--                 forwardSignature = Nothing,
--                 from =
--                   Just
--                     ( User
--                         { canJoinGroups = Nothing,
--                           canReadAllGroupMessages = Nothing,
--                           firstName = "R\926YK",
--                           id = 877072184,
--                           isBot = False,
--                           languageCode = Just "ru",
--                           lastName = Nothing,
--                           supportsInlineQueries = Nothing,
--                           username = Just "einlied"
--                         }
--                     ),
--                 game = Nothing,
--                 groupChatCreated = Nothing,
--                 invoice = Nothing,
--                 leftChatMember = Nothing,
--                 location = Nothing,
--                 mediaGroupId = Nothing,
--                 messageAutoDeleteTimerChanged = Nothing,
--                 messageId = 839,
--                 migrateFromChatId = Nothing,
--                 migrateToChatId = Nothing,
--                 newChatMembers = Nothing,
--                 newChatPhoto = Nothing,
--                 newChatTitle = Nothing,
--                 passportData = Nothing,
--                 photo = Nothing,
--                 pinnedMessage = Nothing,
--                 poll = Nothing,
--                 proximityAlertTriggered = Nothing,
--                 replyMarkup = Nothing,
--                 replyToMessage = Nothing,
--                 senderChat = Nothing,
--                 sticker = Nothing,
--                 successfulPayment = Nothing,
--                 supergroupChatCreated = Nothing,
--                 text = Just "/m@qualifierbot",
--                 venue = Nothing,
--                 viaBot = Nothing,
--                 video = Nothing,
--                 videoNote = Nothing,
--                 voice = Nothing,
--                 voiceChatEnded = Nothing,
--                 voiceChatParticipantsInvited = Nothing,
--                 voiceChatScheduled = Nothing,
--                 voiceChatStarted = Nothing
--               }
--           ),
--       senderChat = Nothing,
--       sticker = Nothing,
--       successfulPayment = Nothing,
--       supergroupChatCreated = Nothing,
--       text = Just "/m@qualifierbot",
--       venue = Nothing,
--       viaBot = Nothing,
--       video = Nothing,
--       videoNote = Nothing,
--       voice = Nothing,
--       voiceChatEnded = Nothing,
--       voiceChatParticipantsInvited = Nothing,
--       voiceChatScheduled = Nothing,
--       voiceChatStarted = Nothing
--     }
