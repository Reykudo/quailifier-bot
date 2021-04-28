{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Bot.UpdateHandler where

import Bot.CommandHandler (safeHandleDirectMessage)
import qualified Bot.Models as MDLS
import Config (AppT (AppT), Config)
import Control.Monad (void)
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Logger (logDebugNS)
import Control.Monad.RWS (MonadReader)
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Int (Int64)
import qualified Data.Text as T
import Database.Persist ((=.))
import Database.Persist.Postgresql (Entity (entityKey), PersistUniqueWrite (upsertBy), (+=.))
import qualified Web.Telegram.Types as TT
import Web.Telegram.Types.Update

data Note = Note {userTgId :: Int64, chatTgId :: Int64, count :: Int}

createNote :: MonadIO m => Note -> AppT m ()
createNote p = do
  -- increment "createUser"
  logDebugNS "web" "creating a user"
  let Note {userTgId, chatTgId, count} = p
  MDLS.runDb
    ( do
        user <- upsertBy (MDLS.UniqueUserTgId userTgId) (MDLS.User userTgId False) [MDLS.UserTgId =. userTgId]
        chat <- upsertBy (MDLS.UniqueChatTgId chatTgId) (MDLS.Chat chatTgId) [MDLS.ChatTgId =. chatTgId]
        ratingEntity <-
          upsertBy
            ( MDLS.UniqueRating
                (entityKey user)
                (entityKey chat)
            )
            ( ( MDLS.Rating
                  { MDLS.ratingCount = count,
                    MDLS.ratingUser = entityKey user,
                    MDLS.ratingChat = entityKey chat
                  }
              )
            )
            [MDLS.RatingCount +=. count]

        pure ()
    )

  pure ()

--   return $ fromSqlKey newUser
updateHandler update =
  do
    case update of
      Message
        { message =
            TT.Msg
              { TT.metadata =
                  TT.MMetadata
                    { TT.from = Just TT.User {TT.userId = userTgId},
                      TT.chat = TT.Chat {TT.chatId = chatTgId}
                    },
                TT.content = TT.TextM {TT.text = text}
              }
        }
          | chatTgId < 0 -> createNote (Note {userTgId, chatTgId, count = T.length text})
          | chatTgId == userTgId -> void $ safeHandleDirectMessage userTgId text
      -- TT.BC {TT.command} -> undefined
      _ -> liftIO $ print update

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
