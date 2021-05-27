module Utils where

import Control.Exception.Safe
  ( Exception,
    MonadCatch,
    throwIO,
    try,
  )
import Control.Monad.Except (ExceptT (..), runExceptT, (<=<))
import UnliftIO (Exception, MonadUnliftIO (withRunInIO))

instance (MonadUnliftIO m, Exception e, MonadCatch m) => MonadUnliftIO (ExceptT e m) where
  withRunInIO exceptToIO = ExceptT $
    try $
      withRunInIO $ \runInIO ->
        exceptToIO (runInIO . (either throwIO pure <=< runExceptT))
