{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Exception where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (filterM)
import Control.Monad.Except (ExceptT (ExceptT))
import qualified Data.Text as T

data BotException
  = UserNotFound
  | RawText T.Text
  | NotMatched
  | ManyErrors [BotException]
  deriving (Show, Eq)

instance Semigroup BotException where
  ManyErrors l1 <> ManyErrors l2 = ManyErrors (l1 <> l2)
  b <> ManyErrors l = ManyErrors (b : l)
  ManyErrors l <> b = ManyErrors (l <> [b])
  a <> b = ManyErrors [a, b]

instance Monoid BotException where
  mempty = ManyErrors []

instance {-# OVERLAPPING #-} (Monad m) => MonadFail (ExceptT BotException m) where
  fail = ExceptT . pure . Left . RawText . T.pack

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
