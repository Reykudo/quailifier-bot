{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Bot.Handler.ChatMessage where

import Bot.Handler.Common
import qualified Bot.Models as MDLS
import Config (AppT (AppT), Config)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Logger (MonadLogger, logDebugNS)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Int (Int64)
import qualified Data.Text as T
import Database.Persist
import qualified Web.Telegram.Types as TG

handleChatMessage :: (MessageHandlerReader m, MonadLogger m, MonadIO m) => MaybeT m ()
handleChatMessage = do
  -- increment "createUser"
  --   logDebugNS "web" "creating a user"
  (Just TG.User {userId = userTgId}) <- asks $ TG.from . TG.metadata . message
  TG.Chat {chatId = chatTgId} <- asks $ TG.chat . TG.metadata . message
  TG.TextM {text} <- asks $ TG.content . message
  let count = T.length text
  runDbMHE
    ( do
        user <- upsertBy (MDLS.UniqueUserTgId userTgId) (MDLS.User userTgId False) [MDLS.UserTgId =. userTgId]
        chat <- upsertBy (MDLS.UniqueChatTgId chatTgId) (MDLS.Chat chatTgId) [MDLS.ChatTgId =. chatTgId]
        ratingEntity <-
          upsertBy
            ( MDLS.UniqueRating
                (entityKey user)
                (entityKey chat)
            )
            ( ( MDLS.Rating
                  { MDLS.ratingCount = count,
                    MDLS.ratingUser = entityKey user,
                    MDLS.ratingChat = entityKey chat
                  }
              )
            )
            [MDLS.RatingCount +=. count]

        pure ()
    )

  pure ()
