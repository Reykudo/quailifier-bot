{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Bot.Handler.Message where

import Bot.Handler.ChatMessage (handleChatMessage)
import Bot.Handler.Common (replyBack, runMessageHandlerReader)
import Bot.Handler.DirectMessage (handleDirectMessage)
import Bot.Models (Chat (chatTgId), User (userTgId))
import Config (Config (Config))
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard, join, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Foldable (asum, traverse_)
import qualified Data.Text as T
import TgBotAPI.Types (Message)
import TgBotAPI.Types.Chat (Chat (Chat, id))
import TgBotAPI.Types.Message (Message (Message), chat, from, text)
import TgBotAPI.Types.User (User (User, id))

liftMaybe :: Applicative m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . pure

handleMessage :: (MonadReader Config m, MonadLogger m, MonadIO m) => Message -> m ()
handleMessage
  message@Message
    { from = Just User {id = userTgId},
      chat = Chat {id = chatTgId},
      text
    } = do
    let withCondition conditon m = do
          guard conditon
          v <- runMessageHandlerReader message $ runMaybeT m
          MaybeT $ pure v

    runMaybeT $
      asum
        [ withCondition (chatTgId < 0) handleChatMessage,
          withCondition (userTgId == chatTgId) handleDirectMessage,
          withCondition (userTgId == chatTgId) $ replyBack "Not Implemented",
          liftIO $ putStrLn "NotMatched"
        ]
    -- ]

    --
    -- let a =
    --       (runMaybeT . runMessageHandlerReader message)
    --         <$> [ lift $ when (chatTgId > 0) ,
    --               -- when (chatTgId == userTgId) handleDirectMessage,
    --               -- when (chatTgId == userTgId) $ replyBack "notMatched",
    --               liftIO $ print $ "Not Matched!" <> show message
    --             ]
    -- asum a
    pure ()
handleMessage m = liftIO $ putStr $ show m <> "\n\n"
