{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Bot.Client where

import Config (Config (Config, configTgMaxHandlers, configToken))
import Control.Applicative (Applicative (pure), (<|>))
import Control.Concurrent (forkIO, newChan)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (SomeException (SomeException), throw, throwIO)
import Control.Monad (join)
import Control.Monad.Cont (forever, replicateM_)
import Control.Monad.Error.Class (MonadError (catchError), liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Control.Retry as Retry
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser, (.:))
import Data.Either (Either (Left, Right), either, isLeft)
import Data.Foldable (Foldable (foldl', length), traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.STRef (STRef, newSTRef)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Network.HTTP.Client (HttpException (HttpExceptionRequest))
import Network.HTTP.Client.Internal (Request (Request))
import Network.HTTP.Client.TLS (newTlsManager)
import Servant
  ( Proxy (..),
    type (:>),
  )
import Servant.API
  ( Capture,
    Get,
    JSON,
    ReqBody,
    type (:<|>) (..),
    type (:>),
  )
import qualified Servant.Client as SC
import Web.Telegram.API
  ( ChatId (ChatId),
    Polling (..),
    SMessage,
    Token,
  )
import qualified Web.Telegram.API as WTA
  ( GetChat,
    GetChatMember,
    GetMe,
    GetMyCommands,
    GetUpdates,
    SendMessage,
    Token (..),
  )
import Web.Telegram.API.Sending.Data
  ( SMessage
      ( SMsg,
        chatId,
        disableNotification,
        disableWebPagePreview,
        parseMode,
        replyMarkup,
        replyToMessageId,
        text
      ),
  )
import Web.Telegram.Types
  ( Chat (Chat, chatId),
    ChatMember,
    Message (..),
    MessageContent (TextM, text),
    MessageMetadata (MMetadata, chat),
    User (firstName, lastName, username),
    from,
  )
import Web.Telegram.Types.Update
  ( ReqResult (..),
    Update
      ( CallbackQuery,
        ChannelPost,
        ChosenInlineResult,
        EditedChannelPost,
        EditedMessage,
        InlineQuery,
        Message,
        PollUpdate,
        PreCheckoutQuery,
        ShippingQuery,
        Unknown,
        message,
        updateId
      ),
  )

type Routes = WTA.SendMessage :<|> WTA.GetChatMember :<|> WTA.GetChat :<|> WTA.GetMyCommands

getUpdateProxy = Proxy @WTA.GetUpdates

methodsProxy = Proxy @Routes

cfg :: SC.BaseUrl
cfg =
  ( SC.BaseUrl
      { SC.baseUrlScheme = SC.Http,
        SC.baseUrlHost = "api.telegram.org",
        SC.baseUrlPort = 80,
        SC.baseUrlPath = ""
      }
  )

getUpdates :: (MonadIO m) => Token -> Polling -> m (ReqResult [Update])
getUpdates =
  SC.hoistClient
    getUpdateProxy
    ( \c -> do
        response <- runReaderT (handleClient c) cfg
        either throw pure response
    )
    (SC.client getUpdateProxy)

-- sendMessage :: (MonadIO m) => Token -> SMessage -> m (ReqResult Message)
-- getChatMember :: (MonadIO m) => (Token -> ChatId -> Int -> m (ReqResult ChatMember))
sendMessage :<|> getChatMember :<|> getChat :<|> getMyCommands = mainClient

-- mainClient :: (MonadIO m) => ((Token -> SMessage -> m (ReqResult Message)) :<|> (Token -> ChatId -> Int -> m (ReqResult ChatMember)))
mainClient =
  SC.hoistClient
    (Proxy :: Proxy Routes)
    ( \c -> do
        response <- runReaderT (handleClient c) cfg
        either (throw) pure response
    )
    (SC.client (Proxy :: Proxy Routes))

handleClient :: (MonadIO m, MonadReader SC.BaseUrl m) => SC.ClientM b -> m (Either SC.ClientError b)
handleClient clientM = do
  manager <- newTlsManager
  clientCfg <- ask
  let clientEnv = SC.mkClientEnv manager clientCfg
  liftIO $
    Retry.retrying
      (Retry.limitRetries 5)
      ( \status response -> do
          TIO.putStrLn $ "status " <> T.pack (show status)
          case response of
            Right b -> pure False
            Left e -> do
              TIO.putStrLn $ "error ocured " <> T.pack (show e)
              case e of
                SC.ConnectionError (SomeException _) -> pure True
                SC.DecodeFailure _ _ -> pure False
                _ -> pure True
      )
      (const $ SC.runClientM clientM clientEnv)

eternalRetry :: (MonadIO m, MonadReader SC.BaseUrl m) => SC.ClientM b -> m (Either T.Text b)
eternalRetry clientM = do
  manager <- newTlsManager
  clientCfg <- ask
  let clientEnv = SC.mkClientEnv manager clientCfg
  response <- liftIO $ SC.runClientM clientM clientEnv
  case response of
    Right b -> pure $ Right b
    Left e -> case e of
      SC.DecodeFailure t _ -> pure $ liftEither $ Left t
      _ -> eternalRetry clientM
