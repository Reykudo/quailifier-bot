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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Network.HTTP.Client (Response (responseBody))
import TgBotAPI.Common
import TgBotAPI.Operations.PostGetUpdates
import TgBotAPI.Types.Update

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

updateLoop :: Config -> (Update -> IO ()) -> IO ()
updateLoop cfg handle = do
  let token = configToken cfg
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

        response <-
          runWithConfiguration
            undefined
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
        PostGetUpdatesResponse200 v <- pure $ responseBody response
        let res = result v
        traverse_ (forkIO . threadHandler) res

        putMVar currentOffset $ foldl' (flip (max . Just . updateId)) Nothing res

        replicateM_ (length res) $ readChan threadsCount
    )
