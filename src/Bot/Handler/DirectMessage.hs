{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Bot.Handler.DirectMessage where

import Bot.Client (getChat, sendMessage)
import Bot.Handler.Common
import qualified Bot.Models as MD
import Config (AppT, Config (Config, configToken))
import Control.Applicative (Alternative, liftA)
import Control.Exception (Exception, catch, throw)
import Control.Exception.Safe (MonadThrow, SomeException (SomeException), throwIO, throwM, try)
import Control.Monad (void)
import Control.Monad.Cont (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.Except (MonadError (catchError, throwError), liftEither, runExceptT)
import Control.Monad.Logger (MonadLogger, logDebugNS, logErrorN)
import Control.Monad.RWS (MonadIO, MonadReader (ask), MonadWriter, guard)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), asks)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Coerce (coerce)
import Data.Either.Combinators (isRight, maybeToRight)
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
    SqlPersistT,
    (==.),
  )
import Web.Telegram.API (ChatId (ChatId))
import Web.Telegram.API.Sending.Data
import qualified Web.Telegram.Types as TG

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

runDbAdapted :: (MessageHandlerReader m, MonadIO m) => SqlPersistT IO b -> m b
runDbAdapted query = do
  cfg <- asks config
  runReaderT (MD.runDb query) cfg

handleDirectMessage' :: (MessageHandlerReader m, MonadLogger m, MonadIO m, MonadError DMException m) => MaybeT m ()
handleDirectMessage' = do
  cfg@Config {configToken} <- asks config
  Just TG.User {userId = userTgIdV} <- asks $ TG.from . TG.metadata . message

  logDebugNS "web" "handle comand"
  user' <- runDbAdapted $ selectFirst [MD.UserTgId ==. userTgIdV] []
  userEntity@Entity {entityVal} <- case user' of
    Nothing -> throwError UserNotFound
    Just r -> pure r
  TG.Msg {} <- asks message
  TG.TextM {text} <- asks $ TG.content . message
  reply userEntity $ dmFromText text

  liftIO $ print $ show entityVal
  pure ()

handleDirectMessage :: (MessageHandlerReader m, MonadLogger m, MonadIO m) => MaybeT m ()
handleDirectMessage = do
  v <- runExceptT $ runMaybeT handleDirectMessage'
  case v of
    Right r -> pure ()
    Left e -> do
      logErrorN $ T.pack $ show e
  guard $ isRight v
  pure ()

-- reply :: (MonadLogger m, MonadReader Config m, MonadIO m) => ChatId -> DMCommand -> m ()
-- reply :: Entity MD.User -> DMCommand -> m ()

-- reply :: MonadIO m => Entity MD.User -> DMCommand -> ReaderT Config m ()
-- reply :: (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend) => Entity MD.User -> p -> ReaderT Config m ()
-- reply :: MonadReader Config m => Entity MD.User -> p -> m ()2

reply :: (MessageHandlerReader m, MonadIO m) => Entity MD.User -> DMCommand -> m ()
reply user command = do
  configToken <- asks $ configToken . config
  TG.Msg {metadata = TG.MMetadata {messageId}} <- asks message
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
                  replyToMessageId = Just $ fromIntegral messageId,
                  replyMarkup = Nothing
                }
            )

  case command of
    GetMe -> do
      let Entity {entityKey = userId} = user
      ratings <- runDbAdapted $ selectList [MD.RatingUser ==. userId] []
      traverse_ (sendBack . T.pack . show . MD.ratingCount . entityVal) ratings
    _ -> void $ sendBack "Not implemented"

-- reportException :: (MonadLogger m, MonadReader Config m, MonadIO m, MonadError DMException m) => Text -> DMException -> m ()
reportException :: (MonadLogger m, MessageHandlerReader m, MonadIO m) => DMException -> MaybeT m ()
reportException e = do
  Config {configToken} <- asks config
  Just TG.User {TG.userId = userId} <- asks $ TG.from . TG.metadata . message
  liftIO $
    sendMessage
      configToken
      ( SMsg
          { chatId = ChatId userId,
            text = errorText e,
            disableWebPagePreview = Nothing,
            parseMode = Nothing,
            disableNotification = Nothing,
            replyToMessageId = Nothing,
            replyMarkup = Nothing
          }
      )

  pure ()

-- someMethod :: (MonadIO m, MonadError String m) => m ()
-- someMethod = do
--   throwError "10"
--   pure ()

-- someMethodSafe :: IO String
-- someMethodSafe = do
--   res <- runExceptT someMethod
--   case res of
--     Left e -> pure ""
--     Right r -> pure ""
