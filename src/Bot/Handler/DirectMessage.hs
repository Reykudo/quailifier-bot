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

module Bot.Handler.DirectMessage where

-- import Bot.Client (getChat, sendMessage)
import Bot.Handler.Common
import qualified Bot.Models as MD
import Config (AppT, Config (Config, configToken), getMethodConfiguretion, runMethod)
import Control.Applicative (Alternative ((<|>)), liftA)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (Exception, catch, throw)
import Control.Exception.Safe (MonadThrow, SomeException (SomeException), throwIO, throwM, try)
import Control.Monad (void)
import Control.Monad.Cont (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.Except (MonadError (catchError, throwError), liftEither, runExceptT)
import Control.Monad.Logger (MonadLogger, logDebugNS, logErrorN)
import Control.Monad.RWS (MonadIO, MonadReader (ask), MonadWriter, guard)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), asks)
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
import Database.Esqueleto ((^.))
import qualified Database.Esqueleto as S
import qualified Database.Persist as DP
import Database.Persist.Postgresql
  ( BackendCompatible,
    Entity (Entity, entityKey, entityVal),
    PersistQueryRead (selectFirst),
    PersistRecordBackend,
    PersistStoreRead (get),
    SqlPersistT,
  )
import Network.HTTP.Client.Internal (Response (Response, responseBody))
import TgBotAPI.Common (Configuration (Configuration, configBaseURL, configSecurityScheme))
import TgBotAPI.Operations.PostGetChat (ChatIdVariants (ChatIdInt), PostGetChatRequestBody (PostGetChatRequestBody, chatId), PostGetChatResponse (PostGetChatResponse200), PostGetChatResponseBody200 (PostGetChatResponseBody200), postGetChatWithConfiguration, result)
import TgBotAPI.Operations.PostSendMessage (ChatIdVariants (ChatIdInt), PostSendMessageRequestBody (PostSendMessageRequestBody), allowSendingWithoutReply, chatId, disableNotification, disableWebPagePreview, entities, parseMode, replyMarkup, replyToMessageId)
import TgBotAPI.Types.Chat (Chat (Chat, id), title)
import TgBotAPI.Types.Message (Message (Message), from, messageId, text)
import TgBotAPI.Types.User (User (User, id))

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
  Just User {id = userTgIdV} <- asks $ from . message

  logDebugNS "web" "handle comand"
  user' <- runDbAdapted $ selectFirst [MD.UserTgId DP.==. userTgIdV] []
  userEntity@Entity {entityVal} <- case user' of
    Nothing -> throwError UserNotFound
    Just r -> pure r
  -- TG.Msg {} <- asks message
  Message {text} <- asks message
  reply userEntity $ dmFromText text

  liftIO $ print $ show entityVal
  pure ()

handleDirectMessage :: (MessageHandlerReader m, MonadLogger m, MonadIO m) => MaybeT m ()
handleDirectMessage = do
  v <- runExceptT $ runMaybeT handleDirectMessage'
  case v of
    Right r -> pure ()
    Left e ->
      logErrorN $ T.pack $ show e
  guard $ isRight v
  pure ()

-- reply :: (MonadLogger m, MonadReader Config m, MonadIO m) => ChatId -> DMCommand -> m ()
-- reply :: Entity MD.User -> DMCommand -> m ()

-- reply :: MonadIO m => Entity MD.User -> DMCommand -> ReaderT Config m ()
-- reply :: (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend) => Entity MD.User -> p -> ReaderT Config m ()
-- reply :: MonadReader Config m => Entity MD.User -> p -> m ()2
selectChatMatchedWith ::
  ( MonadIO m,
    S.BackendCompatible S.SqlBackend backend,
    PersistQueryRead backend,
    S.PersistUniqueRead backend
  ) =>
  S.Key MD.User ->
  ReaderT backend m [Entity MD.Chat]
selectChatMatchedWith userId =
  S.select $
    S.distinct $ S.from \(chat, rating) -> do
      S.where_ ((rating ^. MD.RatingUser) S.==. S.val userId)
      S.where_ ((rating ^. MD.RatingChat) S.==. chat S.^. MD.ChatId)
      pure chat

reply :: (MessageHandlerReader m, MonadIO m, MonadFail m) => Entity MD.User -> DMCommand -> MaybeT m ()
reply user command = do
  configToken <- asks $ configToken . config
  Message {messageId} <- asks message
  case command of
    GetMe -> do
      let Entity {entityKey = userId} = user
      ratings <- runDbAdapted $ DP.selectList [MD.RatingUser DP.==. userId] []
      chats <- runDbAdapted $ selectChatMatchedWith userId
      --  TODO пересмотреть параллельный запуск getChat
      mc <- skipMHE getMethodConfiguretion
      tgChats' <- liftIO $ mapConcurrently (\Entity {entityVal = MD.Chat chatTgId} -> postGetChatWithConfiguration mc PostGetChatRequestBody {chatId = TgBotAPI.Operations.PostGetChat.ChatIdInt chatTgId}) chats
      let tgChats = (\Response {responseBody = PostGetChatResponse200 PostGetChatResponseBody200 {result = r}} -> r) <$> tgChats'
      replyBack $ intercalate "\n" $ makeReport <$> zip ratings tgChats
      pure ()
    _ -> void $ replyBack "Not implemented"
  where
    makeReport (Entity _ MD.Rating {MD.ratingCount = ratingCount}, Chat {id = chatId, title}) =
      T.pack $ "Rating: " <> show ratingCount <> " in chat: " <> T.unpack (fromMaybe " - " title) <> " (" <> show chatId <> ") "

-- reportException :: (MonadLogger m, MonadReader Config m, MonadIO m, MonadError DMException m) => Text -> DMException -> m ()
-- reportException :: (MonadLogger m, MessageHandlerReader m, MonadIO m) => DMException -> MaybeT m ()
-- reportException e = do
--   Config {configToken} <- asks config

--   Just User {id = userId} <- asks $ from . message

--   replyBack

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
