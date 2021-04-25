{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Bot.UpdateLoop where

import Config (Config (Config, configTgMaxHandlers, configToken))
import Control.Applicative (Applicative (pure), (<|>))
import Control.Concurrent (forkIO, newChan)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (SomeException (SomeException), throwIO)
import Control.Monad (join)
import Control.Monad.Cont (forever, replicateM_)
import qualified Control.Retry as Retry
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser, (.:))
import Data.Either (Either (Left, Right), either, isLeft)
import Data.Foldable (Foldable (foldl', length), traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.STRef (STRef, newSTRef)
import Data.Text (Text, pack)
import Data.Text.IO (putStr, putStrLn)
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
  ( GetMe,
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
import Prelude (Bool (False, True), Eq, Functor (fmap), IO, Int, Ord (max, (<)), Semigroup ((<>)), Show (show), Traversable (traverse), flip, fromIntegral, print, sequence, undefined, ($), (+), (-), (.), (<$>))

type Routes = WTA.GetUpdates

getUpdates =
  SC.hoistClient
    (Proxy :: Proxy Routes)
    handleClient
    (SC.client (Proxy :: Proxy Routes))

handleClient :: SC.ClientM b -> IO b
handleClient clientM = do
  manager <- newTlsManager
  let clientEnv =
        SC.mkClientEnv
          manager
          ( SC.BaseUrl
              { SC.baseUrlScheme = SC.Http,
                SC.baseUrlHost = "api.telegram.org",
                SC.baseUrlPort = 80,
                SC.baseUrlPath = ""
              }
          )
  eResponse <-
    Retry.retrying
      (Retry.limitRetries 999999)
      ( \status response -> do
          putStrLn $ "status " <> pack (show status)
          case response of
            Right b -> pure False
            Left e -> do
              putStrLn $ "error ocured " <> pack (show e)
              case e of
                SC.ConnectionError (SomeException _) -> pure True
                SC.DecodeFailure _ _ -> pure False
                _ -> pure True
      )
      ( \r ->
          SC.runClientM clientM clientEnv
      )

  either throwIO pure eResponse

-- outputQueueHandler :: Chan (Maybe Text) -> IO ()
-- outputQueueHandler queue =
--   forever
--     ( do
--         out <- readChan queue
--         case out of
--           Just t -> putStrLn t
--           _ -> pure ()
--     )

-- outputExecutor :: Chan (IO ()) -> IO ()
-- outputExecutor queue =
--   forever
--     ( -- print "qwe"
--       join $ readChan queue
--     )

getUpdateId :: Update -> Maybe Int64
getUpdateId update = case update of
  Message {updateId} -> Just updateId
  EditedMessage {updateId} -> Just updateId
  ChannelPost {updateId} -> Just updateId
  CallbackQuery {updateId} -> Just updateId
  ChosenInlineResult {updateId} -> Just updateId
  InlineQuery {updateId} -> Just updateId
  EditedChannelPost {updateId} -> Just updateId
  ShippingQuery {updateId} -> Just updateId
  PreCheckoutQuery {updateId} -> Just updateId
  PollUpdate {updateId} -> Just updateId
  Unknown {updateId} -> Just updateId

updateLoop :: Config -> (Update -> IO ()) -> IO ()
updateLoop cfg handle = do
  let token = configToken cfg
  (WTA.Token tokenStr) <- pure token
  -- putStrLn $ "Program start with token:" <> tokenStr
  let maxThreads = configTgMaxHandlers cfg
  -- resChannel <- newChan @(Chan ())
  threadsCount <- newChan
  currentOffset <- newMVar (Nothing @Int64)
  -- forkIO $ outputExecutor resChannel
  -- mainLoopSync <- newMVar ()
  let threadHandler = \u ->
        ( do
            handle u
            writeChan threadsCount ()
        )

  forever
    ( do
        -- putStrLn "Loop start"
        offset <- takeMVar currentOffset
        let newOffset = (+ 1) <$> offset
        -- putStrLn $ "newOffset: " <> pack (show offset)

        Ok res <- getUpdates token (Polling {offset = newOffset, limit = Just maxThreads, allowedUpdates = Nothing, timeout = Just 500})
        -- putStrLn $ pack $ show res

        traverse_ (forkIO . threadHandler) res

        putMVar currentOffset $ foldl' (flip (max . getUpdateId)) Nothing res

        replicateM_ (length res) $ readChan threadsCount
    )
