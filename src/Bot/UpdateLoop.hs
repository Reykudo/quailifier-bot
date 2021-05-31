{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Bot.UpdateLoop where

import Bot.Client (runMethod)
import Bot.Exception (BotException)
import Bot.Message.Common (MessageEnvT (MessageEnvT))
import Config (App, AppT (runAppT), Config (Config, configTgMaxHandlers, configToken), getMethodConfiguration)
import Control.Applicative (Applicative (pure), (<|>))
import Control.Exception (SomeException (SomeException), throw, throwIO)
import Control.Monad (join)
import Control.Monad.Cont (MonadIO (liftIO), MonadTrans (lift), forever, replicateM_)
import Control.Monad.Except (ExceptT (..), MonadError, runExceptT)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser, (.:))
import Data.Either (Either (Left, Right), either, isLeft)
import Data.Foldable (Foldable (foldl', length), traverse_)
import Data.Int (Int64)
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Network.HTTP.Client (Response (responseBody))
import qualified Network.HTTP.Client as HS
import Network.HTTP.Client.Internal (Response (Response))
import TgBotAPI.Common
import TgBotAPI.Operations.PostGetUpdates
import TgBotAPI.Types.Update
import UnliftIO (MonadUnliftIO, UnliftIO (unliftIO), newChan, newMVar, putMVar, readChan, takeMVar, tryIO, writeChan)
import UnliftIO.Concurrent (forkIO)

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

updateLoop :: (Update -> App ()) -> App ()
updateLoop handle = do
  cfg <- ask
  let token = configToken cfg
  -- putStrLn $ "Program start with token:" <> tokenStr
  let maxThreads = configTgMaxHandlers cfg
  -- resChannel <- newChan @(Chan ())
  liftIO $ putStrLn "Start updateLoop"
  threadsCount <- newChan
  currentOffset <- newMVar (Nothing @Int64)
  -- forkIO $ outputExecutor resChannel
  -- mainLoopSync <- newMVar ()
  let threadHandler = \u ->
        ( do
            handle u
            writeChan threadsCount ()
        )

  forever $ do
    liftIO $ putStrLn "Loop start"
    offset <- takeMVar currentOffset
    let newOffset = (+ 1) <$> offset

    liftIO $ putStrLn "start update response"
    response <-
      runExceptT $
        runMethod
          ( postGetUpdates
              ( PostGetUpdatesRequestBody
                  { offset = newOffset,
                    limit = Just maxThreads,
                    allowedUpdates = Nothing,
                    timeout = Just 500
                  }
              )
          )
    -- putStrLn $ pack $ show res
    case response of
      Right Response {responseBody = PostGetUpdatesResponse200 v} -> do
        let res = result v

        traverse_ (forkIO . threadHandler) res
        putMVar currentOffset $ foldl' (flip (max . Just . updateId)) Nothing res

        replicateM_ (length res) $ readChan threadsCount
      _ -> do
        liftIO $ putStrLn $ "Error!: " <> show response
        putMVar currentOffset newOffset

    pure ()
