{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Bot.CommandHandler where

import Bot.Client (getChat, sendMessage)
import qualified Bot.Models as MD
import Config (AppT, Config (Config, configToken))
import Control.Applicative (Alternative, liftA)
import Control.Exception (Exception, catch, throw)
import Control.Exception.Safe (MonadThrow, SomeException (SomeException), throwIO, throwM, try)
import Control.Monad (void)
import Control.Monad.Cont (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.Except (MonadError (catchError, throwError), liftEither, runExceptT)
import Control.Monad.Logger (MonadLogger, logDebugNS)
import Control.Monad.RWS (MonadIO, MonadReader (ask), guard)
import Control.Monad.Reader (ReaderT (ReaderT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Coerce (coerce)
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (PersistUniqueRead (getBy), selectFirst, selectList, (==.))
import Database.Persist.Postgresql
  ( Entity (Entity, entityKey, entityVal),
    PersistQueryRead (selectFirst),
    PersistRecordBackend,
    PersistStoreRead (get),
    (==.),
  )
import Web.Telegram.API (ChatId (ChatId))
import Web.Telegram.API.Sending.Data
import qualified Web.Telegram.Types as TG (User)

data DMCommand = Start | Unknown | Subscribe | Unscribe | GetTop | GetMe
  deriving (Show, Eq)

data DMException = UserNotFound deriving (Show, Eq, Exception)

dmFromText :: (Eq a, IsString a) => a -> DMCommand
dmFromText text = case text of
  "/start" -> Start
  "/subscribe" -> Unscribe
  "/unscribe" -> Subscribe
  "/getMe" -> GetMe
  _ -> Unknown

replyMsg :: IsString a => DMCommand -> Maybe a
replyMsg = \case
  Start -> Just "hi"
  _ -> Nothing

errorText :: DMException -> Text
errorText = \case
  UserNotFound -> "Not Found"

-- handleDirectMessage :: (MonadLogger m, MonadReader Config m, MonadIO m, MonadThrow m) => Int64 -> Text -> m ()
handleDirectMessage userTgIdV text = do
  Config {configToken} <- ask
  logDebugNS "web" "handle comand"
  user' <- MD.runDb $ selectFirst [MD.UserTgId ==. userTgIdV] []
  userEntity@Entity {entityVal} <- case user' of
    Nothing -> throwError UserNotFound
    Just r -> pure r

  -- reply userEntity $ text

  -- liftIO $ print $ show entityVal
  pure ()

-- safeHandleDirectMessage :: (MonadReader Config m, MonadLogger m, MonadIO m, Eq a, IsString a) => Int64 -> a -> m ()
safeHandleDirectMessage userTgId text = do
  e <- runExceptT $ handleDirectMessage userTgId text
  case e of
    Left e -> reportException (ChatId userTgId) e
    Right r -> pure r

-- reply :: (MonadLogger m, MonadReader Config m, MonadIO m) => ChatId -> DMCommand -> m ()
-- reply :: Entity MD.User -> DMCommand -> m ()

-- reply :: MonadIO m => Entity MD.User -> DMCommand -> ReaderT Config m ()
-- reply :: (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend) => Entity MD.User -> p -> ReaderT Config m ()
-- reply :: MonadReader Config m => Entity MD.User -> p -> m ()
reply :: (MonadReader Config m, MonadIO m) => Entity MD.User -> Text -> m ()
reply user command = do
  Config {configToken} <- ask
  let sendBack = \text ->
        liftIO $
          sendMessage
            configToken
            ( SMsg
                { chatId = ChatId $ MD.userTgId $ entityVal user,
                  text = text,
                  disableWebPagePreview = Nothing,
                  parseMode = Nothing,
                  disableNotification = Nothing,
                  replyToMessageId = Nothing,
                  replyMarkup = Nothing
                }
            )
  -- res <- getChat configToken $ ChatId $ read $ T.unpack command
  sendBack $ T.pack $ show user
  pure ()

-- case command of
--   GetMe -> do
--     let Entity {entityKey = userId} = user
--     ratings <- MD.runDb $ selectList [MD.RatingUser ==. userId] []
--     traverse_ (sendBack . T.pack . show . MD.ratingCount . entityVal) ratings
--   _ -> void $ sendBack "Not implemented"

-- reportException :: (MonadLogger m, MonadReader Config m, MonadIO m, MonadError DMException m) => Text -> DMException -> m ()
reportException :: (MonadLogger m, MonadReader Config m, MonadIO m) => ChatId -> DMException -> m ()
reportException userTgId e = do
  Config {configToken} <- ask

  liftIO $
    sendMessage
      configToken
      ( SMsg
          { chatId = userTgId,
            text = errorText e,
            disableWebPagePreview = Nothing,
            parseMode = Nothing,
            disableNotification = Nothing,
            replyToMessageId = Nothing,
            replyMarkup = Nothing
          }
      )

  pure ()

--   guard (isJust user)
--   pure ()
