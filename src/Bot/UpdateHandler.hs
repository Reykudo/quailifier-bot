{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Bot.UpdateHandler where

import qualified Bot.Models as MDLS
import Config (AppT (AppT), Config)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Logger (logDebugNS)
import Control.Monad.RWS (MonadReader)
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Int (Int64)
import qualified Data.Text as T
import Database.Persist (BaseBackend, Entity (entityKey), PersistEntity (PersistEntityBackend), PersistEntityBackend, PersistQueryRead (selectFirst), PersistQueryWrite (updateWhere), PersistStoreWrite (insert), PersistUniqueRead (getBy), insertEntity, (==.))
import Database.Persist.Postgresql (SqlBackend, SqlPersistT, fromSqlKey, transactionSave)
import qualified Web.Telegram.Types as TT
import Web.Telegram.Types.Update

data Note = Note {userTgId :: Int, chatTgId :: Int, count :: Int}

getOrCreate :: (PersistStoreWrite backend, PersistEntity e, MonadIO m, PersistEntityBackend e ~ BaseBackend backend) => ReaderT backend m (Maybe (Entity e)) -> e -> ReaderT backend m (Entity e)
getOrCreate byFieldVal fbEntity =
  do
    user <- byFieldVal
    case user of
      Just u -> pure u
      Nothing -> insertEntity fbEntity

createNote :: MonadIO m => Note -> AppT m Int64
createNote p = do
  -- increment "createUser"
  logDebugNS "web" "creating a user"
  let Note {userTgId, chatTgId, count} = p
  MDLS.runDb
    ( do
        
        user <- getOrCreate (getBy $ MDLS.UniqueUserTgId userTgId) (MDLS.User userTgId)
        chat <- getOrCreate (getBy $ MDLS.UniqueChatTgId chatTgId) (MDLS.Chat chatTgId)
        rating <-
          getOrCreate
            (getBy $ MDLS.UniqueRating (entityKey user) (entityKey chat))
            ( MDLS.Rating
                { MDLS.ratingCount = count,
                  MDLS.ratingUser = entityKey user,
                  MDLS.ratingChat = entityKey chat
                }
            )
        pure ()
    )

  pure 0

--   return $ fromSqlKey newUser
updateHandler :: MonadIO m => Update -> AppT m ()
updateHandler update =
  do
    case update of
      Message
        { message =
            TT.Msg
              { TT.metadata =
                  TT.MMetadata
                    { TT.from = Just TT.User {TT.userId = userId},
                      TT.chat = TT.Chat {TT.chatId = chatId}
                    },
                TT.content = TT.TextM {TT.text = text}
              }
        } -> createNote (Note {userTgId =  userId, chatTgId = userId, count = T.length text})
      _ -> pure 0
    pure ()

-- showMsg :: UpdateOrFallback -> Maybe Text
-- showMsg (RealUpdate Message {message = Msg {metadata = MMetadata {from}, content = TextM {text}}}) =
--   pure $ showUser from <> ": " <> text
-- showMsg (RealUpdate Message {message = Msg {metadata = MMetadata {from}}}) = pure $ showUser from <> " - not supported type - "
-- showMsg _ = Nothing

-- showUser :: Maybe User -> Text
-- showUser user =
--   fromMaybe
--     " - "
--     ( do
--         user' <- user
--         ("@" <>) <$> username user' <|> pure (firstName user' <> fromMaybe "" (lastName user'))
--     )

-- send :: Token -> Update -> IO ()
-- send token Message {message = Msg {content = TextM {text}}} =
--   do
--     tid <- myThreadId
--     print tid
--     sendMessage
--       token
--       ( SMsg
--           { chatId = ChatId 877072184,
--             text,
--             disableWebPagePreview = Nothing,
--             parseMode = Nothing,
--             disableNotification = Nothing,
--             replyToMessageId = Nothing,
--             replyMarkup = Nothing
--           }
--       )
--     pure ()
-- send _ _ = pure ()

-- -- a (SC.DecodeFailure b s) = undefined
-- -- cc :: SMessage -> ()
-- -- cc = const ()

-- -- -- 877072184
-- -- a =
-- --   Msg
-- --     {  metadata =
-- --         MMetadata
-- --            {  messageId = 44,
--  --             from  =
-- --               Just
-- --                 ( User
-- --                     {  userId = 877072184,
--  --                       isBot  = False,
-- --                       firstName = "R\926YK",
-- --                       lastName = Nothing,
-- --                       username = Nothing,
-- --                       languageCode = Just "ru",
-- --                       canJoinGroups = Nothing,
-- --                       canReadAllGroupMessages = Nothing,
-- --                       supportsInlineQueries = Nothing
-- --                     }
-- --                 ),
-- --             date = 1617139280,
-- --             chat = Chat { chatId = 877072184, chatType  = Private , title = Nothing, username = Nothing, firstName = Just "R\926YK", lastName = Nothing, photo = Nothing, description = Nothing, inviteLink = Nothing, pinnedMessage = Nothing, permissions = Nothing, slowModeDelay = Nothing, stickerSetName = Nothing, canSetStickerSet = Nothing},
-- --             forwardFrom = Nothing,
-- --             forwardFromChat = Nothing,
-- --             forwardFromMessageId = Nothing,
-- --             forwardSignature = Nothing,
-- --             forwardSenderName = Nothing,
-- --             forwardDate = Nothing,
-- --             replyToMessage = Nothing,
-- --             editDate = Nothing,
-- --             mediaGroupId = Nothing,
-- --             authorSignature = Nothing,
-- --             replyMarkup = Nothing
-- --           },
-- --       content = TextM { text = "q", entities  = Nothing }
-- --     }

-- -- -- b = v a where
-- -- --         v Msg a b c = a
