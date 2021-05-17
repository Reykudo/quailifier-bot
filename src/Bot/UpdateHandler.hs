{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Bot.UpdateHandler where

import Bot.Handler.Message (handleMessage)
import qualified Bot.Models as MDLS
import Config (AppT (AppT), Config)
import Control.Monad (void)
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger, logDebugNS)
import Control.Monad.RWS (MonadReader)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import Data.Int (Int64)
import qualified Data.Text as T
import Database.Persist ((=.))
import Database.Persist.Postgresql (Entity (entityKey), PersistUniqueWrite (upsertBy), (+=.))
import TgBotAPI.Types.Message (Message (Message))
import TgBotAPI.Types.Update (Update (Update), message)

--   return $ fromSqlKey newUser
updateHandler :: (MonadReader Config m, MonadLogger m, MonadIO m) => Update -> m ()
updateHandler Update {message = Just message} = handleMessage message
updateHandler update = liftIO $ print update

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
