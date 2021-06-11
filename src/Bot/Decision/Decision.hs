{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Bot.Decision.Decision where

import qualified Bot.DbModels as DB
-- import Database.Esqueleto

import Bot.Message.Common (sendMessage)
import Bot.Models (DecisionStatus (Process))
import Config (App, Config (Config))
import Control.Monad (forever)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader
import Data.Foldable (traverse_)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Database.Esqueleto.Experimental
import UnliftIO (UnliftIO (unliftIO))
import UnliftIO.Concurrent (forkIO, threadDelay)
import Utils (liftMaybe)

minuteMcS :: Int
minuteMcS = 6000000 * 2

-- allProcessedDecisions ::
--   ( MonadIO m,
--     BackendCompatible SqlBackend backend,
--     PersistQueryRead backend,
--     PersistUniqueRead backend
--   ) =>
--   UTCTime ->
--   ReaderT backend m [Entity DB.Decision]

-- allProcessedDecisions :: (MonadIO m, BackendCompatible SqlBackend backend, PersistQueryRead backend, PersistUniqueRead backend) => UTCTime -> ReaderT backend m [Entity DB.User]
allProcessedDecisions ::
  ( MonadIO m,
    BackendCompatible SqlBackend backend,
    PersistQueryRead backend,
    PersistUniqueRead backend
  ) =>
  UTCTime ->
  ReaderT backend m [(Entity DB.User, Entity DB.Decision, Entity DB.User)]
allProcessedDecisions timeExpire = select $ distinct do
  (initUser :& decision :& targetUser) <-
    from $
      table @DB.User
        `innerJoin` table @DB.Decision `on` (\(initUser :& decision) -> (decision ^. DB.DecisionInitUser ==. initUser ^. DB.UserId))
        `innerJoin` table @DB.User `on` (\(_ :& decision :& targetUser) -> decision ^. DB.DecisionTargetUser ==. targetUser ^. DB.UserId)
  where_
    (decision ^. DB.DecisionStatus ==. val Process)
  where_
    (decision ^. DB.DecisionInitDate <=. val timeExpire)
  pure (initUser, decision, targetUser)

getAllSubscribed chatId = select $ do
  rating <- from $ table @DB.Rating
  user <- from $ table @DB.User
  where_ $ rating ^. DB.RatingChat ==. val chatId
  where_ $ rating ^. DB.RatingUser ==. user ^. DB.UserId
  where_ $ user ^. DB.UserSubscribed ==. val True
  pure user

makeDecision :: (Entity DB.User, Entity DB.Decision, Entity DB.User) -> App ()
makeDecision
  ( initUserEntity,
    decisionEntity@Entity {entityKey = decisionKey, entityVal = decision},
    targetUserEntity
    ) = do
    let chatId = DB.decisionChat $ decision
    chat' <- DB.runDb $ getEntity chatId
    case chat' of
      Nothing -> pure ()
      Just chat -> do
        users <- DB.runDb $ getAllSubscribed $ entityKey chat
        traverse_ handle users
        where
          handle userEntity@Entity {entityKey = userKey, entityVal = user} = do
            sendMessage (DB.userTgId user) "~"
            DB.runDb $ do upsertBy (DB.UniqueUserDecision userKey decisionKey) (DB.UserDecision userKey decisionKey False) []
            pure ()

makeDecisions :: App ()
makeDecisions = do
  now <- liftIO getCurrentTime
  let timeExpire = addUTCTime (secondsToNominalDiffTime (-3600)) now
  pairs <- DB.runDb $ allProcessedDecisions timeExpire
  traverse_ makeDecision pairs

startSheduler :: App ()
startSheduler = void $
  forkIO . forever $ do
    makeDecisions
    liftIO $ threadDelay minuteMcS
    pure ()

-- a =
--   PostSendMessageResponse200
--     ( PostSendMessageResponseBody200
--         { ok = True,
--           result =
--             Message
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
--                       firstName = Just "R\926YK",
--                       id = 877072184,
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
--                       title = Nothing,
--                       type' = TypeEnumPrivate,
--                       username = Just "einlied"
--                     },
--                 connectedWebsite = Nothing,
--                 contact = Nothing,
--                 date = 1622140513,
--                 deleteChatPhoto = Nothing,
--                 dice = Nothing,
--                 document = Nothing,
--                 editDate = Nothing,
--                 entities = Nothing,
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
--                           firstName = "Nya",
--                           id = 1040202512,
--                           isBot = True,
--                           languageCode = Nothing,
--                           lastName = Nothing,
--                           supportsInlineQueries = Nothing,
--                           username = Just "qualifierbot"
--                         }
--                     ),
--                 game = Nothing,
--                 groupChatCreated = Nothing,
--                 invoice = Nothing,
--                 leftChatMember = Nothing,
--                 location = Nothing,
--                 mediaGroupId = Nothing,
--                 messageAutoDeleteTimerChanged = Nothing,
--                 messageId = 828,
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
--                 replyToMessage =
--                   Just
--                     ( Message
--                         { animation = Nothing,
--                           audio = Nothing,
--                           authorSignature = Nothing,
--                           caption = Nothing,
--                           captionEntities = Nothing,
--                           channelChatCreated = Nothing,
--                           chat =
--                             Chat
--                               { bio = Nothing,
--                                 canSetStickerSet = Nothing,
--                                 description = Nothing,
--                                 firstName = Just "R\926YK",
--                                 id = 877072184,
--                                 inviteLink = Nothing,
--                                 lastName = Nothing,
--                                 linkedChatId = Nothing,
--                                 location = Nothing,
--                                 messageAutoDeleteTime = Nothing,
--                                 permissions = Nothing,
--                                 photo = Nothing,
--                                 pinnedMessage = Nothing,
--                                 slowModeDelay = Nothing,
--                                 stickerSetName = Nothing,
--                                 title = Nothing,
--                                 type' = TypeEnumPrivate,
--                                 username = Just "einlied"
--                               },
--                           connectedWebsite = Nothing,
--                           contact = Nothing,
--                           date = 1622140513,
--                           deleteChatPhoto = Nothing,
--                           dice = Nothing,
--                           document = Nothing,
--                           editDate = Nothing,
--                           entities =
--                             Just
--                               [ MessageEntity
--                                   { language = Nothing,
--                                     length = 6,
--                                     offset = 0,
--                                     type' = TypeEnumBotCommand,
--                                     url = Nothing,
--                                     user = Nothing
--                                   }
--                               ],
--                           forwardDate = Nothing,
--                           forwardFrom = Nothing,
--                           forwardFromChat = Nothing,
--                           forwardFromMessageId = Nothing,
--                           forwardSenderName = Nothing,
--                           forwardSignature = Nothing,
--                           from =
--                             Just
--                               ( User
--                                   { canJoinGroups = Nothing,
--                                     canReadAllGroupMessages = Nothing,
--                                     firstName = "R\926YK",
--                                     id = 877072184,
--                                     isBot = False,
--                                     languageCode = Just "ru",
--                                     lastName = Nothing,
--                                     supportsInlineQueries = Nothing,
--                                     username = Just "einlied"
--                                   }
--                               ),
--                           game = Nothing,
--                           groupChatCreated = Nothing,
--                           invoice = Nothing,
--                           leftChatMember = Nothing,
--                           location = Nothing,
--                           mediaGroupId = Nothing,
--                           messageAutoDeleteTimerChanged = Nothing,
--                           messageId = 827,
--                           migrateFromChatId = Nothing,
--                           migrateToChatId = Nothing,
--                           newChatMembers = Nothing,
--                           newChatPhoto = Nothing,
--                           newChatTitle = Nothing,
--                           passportData = Nothing,
--                           photo = Nothing,
--                           pinnedMessage = Nothing,
--                           poll = Nothing,
--                           proximityAlertTriggered = Nothing,
--                           replyMarkup = Nothing,
--                           replyToMessage = Nothing,
--                           senderChat = Nothing,
--                           sticker = Nothing,
--                           successfulPayment = Nothing,
--                           supergroupChatCreated = Nothing,
--                           text = Just "/start",
--                           venue = Nothing,
--                           viaBot = Nothing,
--                           video = Nothing,
--                           videoNote = Nothing,
--                           voice = Nothing,
--                           voiceChatEnded = Nothing,
--                           voiceChatParticipantsInvited = Nothing,
--                           voiceChatScheduled = Nothing,
--                           voiceChatStarted = Nothing
--                         }
--                     ),
--                 senderChat = Nothing,
--                 sticker = Nothing,
--                 successfulPayment = Nothing,
--                 supergroupChatCreated = Nothing,
--                 text = Just "\1053\1077\1080\1079\1074\1077\1089\1090\1085\1072\1103 \1082\1086\1084\1072\1085\1076\1072",
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
--         }
--     )
