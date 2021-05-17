{-# LANGUAGE BlockArguments #-}
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

-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Bot.Message.DirectMessage where

import Bot.Client (getChat, sendMessage)
import qualified Bot.DbModels as DB
import Bot.Exception (BotExceptT, BotException (NotMatched))
import Bot.Message.Common
import Config (AppT, Config (Config, configToken))
import Control.Applicative (Alternative ((<|>)), liftA)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (Exception, catch, throw)
import Control.Exception.Safe (MonadThrow, SomeException (SomeException), throwIO, throwM, try)
import Control.Monad (void)
import Control.Monad.Cont (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.Except (Except, ExceptT, MonadError (catchError, throwError), liftEither, runExceptT)
import Control.Monad.Logger (MonadLogger, logDebugNS, logErrorN)
import Control.Monad.RWS (MonadIO, MonadReader (ask), MonadWriter, guard)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), asks)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Parallel.Strategies (evalTraversable, rpar, runEval, runEvalIO, withStrategy)
import Data.Coerce (coerce)
import Data.Either.Combinators (isRight, maybeToRight)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.String (IsString)
import Data.Text (Text, intercalate)
import qualified Data.Text as T
import Database.Esqueleto as S
import qualified Database.Persist as DP
import Database.Persist.Postgresql
  ( Entity (Entity, entityKey, entityVal),
    PersistQueryRead (selectFirst),
    PersistRecordBackend,
    PersistStoreRead (get),
    SqlPersistT,
  )
import Web.Telegram.API (ChatId (ChatId))
import Web.Telegram.API.Sending.Data
import qualified Web.Telegram.Types as TG
import Web.Telegram.Types.Update (ReqResult (Ok))

data DMCommand = Start | Unknown | Subscribe | Unscribe | GetTop | GetMe
  deriving (Show, Eq)

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

handleDirectMessage :: (MonadLogger m, MonadIO m, MonadReader MessageHandlerEnv m, MonadFail m, MonadError BotException m) => m ()
handleDirectMessage = do
  cfg@Config {configToken} <- asks config
  Just TG.User {userId = userTgIdV} <- asks $ TG.from . TG.metadata . message

  logDebugNS "web" "handle comand"
  user' <- runDbInMsgEnv $ selectFirst [DB.UserTgId DP.==. userTgIdV] []
  Just userEntity@Entity {entityVal} <- pure user'
  -- TG.Msg {} <- asks message
  TG.TextM {text} <- asks $ TG.content . message
  reply userEntity $ dmFromText text

  liftIO $ print $ show entityVal
  pure ()

-- handleDirectMessage :: (MessageHandlerReader m, MonadLogger m, MonadIO m) => MaybeT m ()
-- handleDirectMessage = do
--   -- a <-
--   v <- runMaybeT handleDirectMessage'

--   -- ExceptT $ pure a
--   case v of
--     Right r -> pure r
--     Left e -> do
--       logErrorN $ T.pack e
--       fail e

-- reply :: (MonadLogger m, MonadReader Config m, MonadIO m) => ChatId -> DMCommand -> m ()
-- reply :: Entity DB.User -> DMCommand -> m ()

-- reply :: MonadIO m => Entity DB.User -> DMCommand -> ReaderT Config m ()
-- reply :: (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend) => Entity DB.User -> p -> ReaderT Config m ()
-- reply :: MonadReader Config m => Entity DB.User -> p -> m ()2
selectChatMatchedWith ::
  ( MonadIO m,
    BackendCompatible SqlBackend backend,
    PersistQueryRead backend,
    PersistUniqueRead backend
  ) =>
  Key DB.User ->
  ReaderT backend m [Entity DB.Chat]
selectChatMatchedWith userId = do
  S.select $
    S.distinct $ S.from \(chat, rating) -> do
      S.where_ ((rating ^. DB.RatingUser) S.==. S.val userId)
      S.where_ ((rating ^. DB.RatingChat) S.==. chat S.^. DB.ChatId)
      pure chat

reply :: (MonadIO m, MonadReader MessageHandlerEnv m, MonadFail m, MonadError BotException m) => Entity DB.User -> DMCommand -> m ()
reply user command = do
  configToken <- asks $ configToken . config
  TG.Msg {metadata = TG.MMetadata {messageId}} <- asks message

  case command of
    GetMe -> do
      let Entity {entityKey = userId} = user
      ratings <- runDbInMsgEnv $ DP.selectList [DB.RatingUser DP.==. userId] []
      chats <- runDbInMsgEnv $ selectChatMatchedWith userId
      tgChats <- liftIO $ mapConcurrently (getChat configToken . ChatId . DB.chatTgId . entityVal) chats

      replyBack $ intercalate "\n" $ makeReport <$> zip ratings tgChats
      pure ()
    _ -> throwError NotMatched
  where
    makeReport (Entity _ DB.Rating {DB.ratingCount = ratingCount}, Ok TG.Chat {chatId, title}) =
      T.pack $ "Rating: " <> show ratingCount <> " in chat: " <> T.unpack (fromMaybe " - " title) <> " (" <> show chatId <> ") "

-- -- reportException :: (MonadLogger m, MonadReader Config m, MonadIO m, MonadError BotException m) => Text -> BotException -> m ()
-- reportException :: (MonadLogger m, MessageHandlerReader m, MonadIO m) => BotException -> BotExceptT m ()
-- reportException e = do
--   Config {configToken} <- asks config

--   Just TG.User {TG.userId = userId} <- asks $ TG.from . TG.metadata . message

--   liftIO $
--     sendMessage
--       configToken
--       ( SMsg
--           { chatId = ChatId userId,
--             text = errorText e,
--             disableWebPagePreview = Nothing,
--             parseMode = Nothing,
--             disableNotification = Nothing,
--             replyToMessageId = Nothing,
--             replyMarkup = Nothing
--           }
--       )

--   pure ()

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