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
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Data.Text as T
import qualified Web.Telegram.Types as TT

handleMessage
  message@TT.Msg
    { TT.metadata =
        TT.MMetadata
          { TT.from = Just TT.User {TT.userId = userTgId},
            TT.chat = TT.Chat {TT.chatId = chatTgId}
          },
      TT.content = TT.TextM {TT.text = text}
    } = do
    sequence_ $
      runMessageHandlerReader message . runMaybeT
        <$> [ when (chatTgId > 0) handleChatMessage,
              when (chatTgId == userTgId) handleDirectMessage,
              when (chatTgId == userTgId) $ replyBack "notMatched",
              liftIO $ print $ "Not Matched!" <> show message
            ]
