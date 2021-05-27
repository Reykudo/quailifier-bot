{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Bot.Exception where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Exception (Exception)
import Control.Monad (filterM)
import Control.Monad.Except (ExceptT (ExceptT), MonadError, mapExceptT, runExceptT)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.Text as T
import qualified Network.HTTP.Client as HS

data BotException
  = UserNotFound
  | RawText T.Text
  | NotMatched
  | NetwortError HS.HttpException
  | ManyErrors [BotException]
  deriving (Show)

deriving instance Exception BotException

-- runWithBotException :: ExceptT HS.HttpException m b -> ExceptT BotException m b
-- fromHttpToBotException :: Show s => s -> BotException

runWithBotException :: (Monad m) => ExceptT HS.HttpException m b -> ExceptT BotException m b
runWithBotException = mapExceptT (first NetwortError <$>)

instance Semigroup BotException where
  ManyErrors l1 <> ManyErrors l2 = ManyErrors (l1 <> l2)
  b <> ManyErrors l = ManyErrors (b : l)
  ManyErrors l <> b = ManyErrors (l <> [b])
  a <> b = ManyErrors [a, b]

instance Monoid BotException where
  mempty = ManyErrors []

-- _ -> ""

firstNotNull :: [a] -> [a] -> [a]
firstNotNull [] a = a
firstNotNull a _ = a

-- errorText :: BotException -> T.Text
makeErrorReport :: BotException -> T.Text
makeErrorReport = \case
  NotMatched -> "Неизвестная команда"
  RawText s -> "Незадокументированная ошибка: " <> s
  UserNotFound -> "Неизвестный пользователь"
  ManyErrors l -> T.intercalate "\n" $ (makeErrorReport <$>) $ (`firstNotNull` [NotMatched]) $ filter (\case NotMatched -> False; _ -> True) l

type BotExceptT = ExceptT BotException
