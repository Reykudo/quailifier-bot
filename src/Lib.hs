{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib where
import Control.Applicative (Applicative (pure), (<|>))
import Control.Concurrent
  ( Chan,
    MVar,
    forkIO,
    myThreadId,
    newChan,
    newMVar,
    putMVar,
    readChan,
    readMVar,
    takeMVar,
    writeChan,
    writeList2Chan,
  )
import Control.Exception (SomeException (SomeException), throwIO)
import Control.Monad.Cont (forever, replicateM_)
import qualified Control.Retry as Retry
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser, (.:))
import Data.Either (Either (Left, Right), either, isLeft)
import Data.Foldable (Foldable (foldl', length))
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Text (Text, pack)
import Data.Text.IO (putStrLn)
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
        message,
        updateId
      ),
  )
import Prelude (Bool (False, True), Eq, Functor (fmap), IO, Int, Ord (max), Semigroup ((<>)), Show (show), print, ($), (+), (<$>))

type Routes = GetUpdates

data UpdateOrFallback = RealUpdate Update | Fallback {updateId :: Int} deriving (Eq, Show)

instance A.FromJSON UpdateOrFallback where
  parseJSON value =
    (RealUpdate <$> (A.parseJSON value :: (Parser Update))) <|> A.withObject "Fallback" (\v -> Fallback <$> (v .: "update_id")) value

type GetUpdates =
  Capture "token" Token
    :> "getUpdates"
    :> ReqBody '[JSON] Polling
    :> Get '[JSON] (ReqResult [UpdateOrFallback])

getUpdates :: Token -> Polling -> IO (ReqResult [UpdateOrFallback])
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
          -- putStrLn $ "status " <> pack (show status)
          case response of
            Right b -> pure False
            Left e -> do
              -- putStrLn $ "error ocured " <> pack (show e)
              case e of
                SC.ConnectionError (SomeException _) -> pure True
                SC.DecodeFailure _ _ -> pure False
                _ -> pure True
      )
      ( \r -> do
          -- print r
          SC.runClientM clientM clientEnv
      )

  either throwIO pure eResponse

mainFn :: IO ()
mainFn =
  do
    let token = ""
    let maxThreads = 15
    channel <- newChan :: (IO (Chan UpdateOrFallback))
    resChannel <- newChan :: (IO (Chan ()))
    currentOffset <- newMVar Nothing
    forever
      ( do
          offset <- readMVar currentOffset
          Ok res <- getUpdates token (Polling {offset = (+ 1) <$> offset, limit = Just maxThreads, allowedUpdates = Nothing, timeout = Just 86399})
          -- print res
          let threadCount = length res
          writeList2Chan channel res
          replicateM_ threadCount $ forkIO $ threadHandler currentOffset channel resChannel showMsg

          replicateM_ threadCount (readChan resChannel)
          maxOffset <- readMVar currentOffset
          -- print maxOffset
          pure ()
      )
  where
    threadHandler :: MVar (Maybe Int) -> Chan UpdateOrFallback -> Chan () -> (UpdateOrFallback -> IO ()) -> IO ()
    threadHandler currentOffsetMVar channel resChannel action = do
      tid <- myThreadId
      -- putStrLn ("thread № " <> pack (show tid) <> " started")

      updateOrFb <- readChan channel
      -- print update
      let currentOffset = case updateOrFb of
            RealUpdate update ->
              ( case update of
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
                  _ -> Nothing
              )
            Fallback {updateId} -> Just updateId

      prevOffset <- takeMVar currentOffsetMVar
      -- print $ "prevOffset" <> show prevOffset
      putMVar currentOffsetMVar $ max currentOffset prevOffset

      action updateOrFb
      -- putStrLn ("thread № " <> pack (show tid) <> " ended")
      writeChan resChannel ()
      pure ()

showMsg :: UpdateOrFallback -> IO ()
showMsg (RealUpdate Message {message = Msg {metadata = MMetadata {from}, content = TextM {text}}}) =
  putStrLn $ showUser from <> ": " <> text
showMsg (RealUpdate Message {message = Msg {metadata = MMetadata {from}}}) = putStrLn $ showUser from <> " - not supported type - "
showMsg _ = pure ()

showUser :: Maybe User -> Text
showUser user =
  fromMaybe
    " - "
    ( do
        user' <- user
        (("@" <>) <$> username user') <|> pure (firstName user' <> fromMaybe "" (lastName user'))
    )

-- send :: Token -> Update -> IO ()
-- send token Message {message = Msg {metadata = MMetadata {chat = Chat {chatId}}, content = TextM {text}}} =
--   do
--     sendMessage
--       token
--       ( SMsg
--           { chatId = ChatId chatId,
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
-- --     { metadata =
-- --         MMetadata
-- --           { messageId = 44,
-- --             from =
-- --               Just
-- --                 ( User
-- --                     { userId = 877072184,
-- --                       isBot = False,
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
-- --             chat = Chat {chatId = 877072184, chatType = Private, title = Nothing, username = Nothing, firstName = Just "R\926YK", lastName = Nothing, photo = Nothing, description = Nothing, inviteLink = Nothing, pinnedMessage = Nothing, permissions = Nothing, slowModeDelay = Nothing, stickerSetName = Nothing, canSetStickerSet = Nothing},
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
-- --       content = TextM {text = "q", entities = Nothing}
-- --     }

-- -- -- b = v a where
-- -- --         v Msg a b c = a
