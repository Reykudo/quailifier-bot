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

import Bot.Exception (BotException (NetwortError))
import Config (Config (configCache), GlobalCaches, getMethodConfiguration)
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, asks)
import Data.Cache
import Data.Hashable (Hashable)
import qualified Network.HTTP.Client as HS
import TgBotAPI.Common (StripeT, runWithConfiguration)
import UnliftIO (MonadIO, try)
import Prelude hiding (lookup)

runMethod :: (Monad m, MonadReader Config m, MonadIO m, MonadError HS.HttpException m) => StripeT IO b -> m b
runMethod method = do
  mc <- getMethodConfiguration
  a <- liftIO $ try (runWithConfiguration mc method)
  case a of
    Left (e :: HS.HttpException) -> throwError e
    Right r -> pure r

runMethodWithCache :: (Monad m, MonadReader Config m, MonadIO m, MonadError HS.HttpException m, Eq a, Hashable a) => (GlobalCaches -> Cache a b) -> a -> (a -> StripeT IO b) -> m b
runMethodWithCache accessor arg method = do
  globalCache <- asks configCache
  let cache = accessor globalCache
  a <- liftIO $ lookup cache arg
  case a of
    Nothing -> do
      v <- runMethod (method arg)
      liftIO $ insert cache arg v
      pure v
    Just j -> pure j

-- type Routes = WTA.SendMessage :<|> WTA.GetChatMember :<|> WTA.GetChat

-- getUpdateProxy = Proxy @WTA.GetUpdates

-- methodsProxy = Proxy @Routes

-- cfg :: SC.BaseUrl
-- cfg =
--   ( SC.BaseUrl
--       { SC.baseUrlScheme = SC.Http,
--         SC.baseUrlHost = "api.telegram.org",
--         SC.baseUrlPort = 80,
--         SC.baseUrlPath = ""
--       }
--   )

-- getUpdates :: (MonadIO m) => Token -> Polling -> m (ReqResult [Update])
-- getUpdates =
--   SC.hoistClient
--     getUpdateProxy
--     ( \c -> do
--         response <- runReaderT (handleClient c) cfg
--         either (error . show) pure response
--     )
--     (SC.client getUpdateProxy)

-- -- sendMessage :: (MonadIO m) => Token -> SMessage -> m (ReqResult Message)
-- -- getChatMember :: (MonadIO m) => (Token -> ChatId -> Int -> m (ReqResult ChatMember))
-- sendMessage :<|> getChatMember :<|> getChat = mainClient

-- -- mainClient :: (MonadIO m) => ((Token -> SMessage -> m (ReqResult Message)) :<|> (Token -> ChatId -> Int -> m (ReqResult ChatMember)))
-- mainClient =
--   SC.hoistClient
--     (Proxy :: Proxy Routes)
--     ( \c -> do
--         response <- runReaderT (handleClient c) cfg
--         either (error . show) pure response
--     )
--     (SC.client (Proxy :: Proxy Routes))

-- handleClient :: (MonadIO m, MonadReader SC.BaseUrl m) => SC.ClientM b -> m (Either SC.ClientError b)
-- handleClient clientM = do
--   manager <- newTlsManager
--   clientCfg <- ask
--   let clientEnv = SC.mkClientEnv manager clientCfg
--   liftIO $
--     Retry.retrying
--       (Retry.limitRetries 5)
--       ( \status response -> do
--           TIO.putStrLn $ "status " <> T.pack (show status)
--           case response of
--             Right b -> pure False
--             Left e -> do
--               TIO.putStrLn $ "error ocured " <> T.pack (show e)
--               case e of
--                 SC.ConnectionError (SomeException _) -> pure True
--                 SC.DecodeFailure _ _ -> pure False
--                 _ -> pure True
--       )
--       (const $ SC.runClientM clientM clientEnv)

-- eternalRetry :: (MonadIO m, MonadReader SC.BaseUrl m) => SC.ClientM b -> m (Either T.Text b)
-- eternalRetry clientM = do
--   manager <- newTlsManager
--   clientCfg <- ask
--   let clientEnv = SC.mkClientEnv manager clientCfg
--   response <- liftIO $ SC.runClientM clientM clientEnv
--   case response of
--     Right b -> pure $ Right b
--     Left e -> case e of
--       SC.DecodeFailure t _ -> pure $ liftEither $ Left t
--       _ -> eternalRetry clientM
