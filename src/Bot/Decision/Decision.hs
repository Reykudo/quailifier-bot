{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Bot.Decision.Decision where

import qualified Bot.DbModels as BM
import Bot.Models (DecisionStatus (Process))
import Config (Config (Config))
import Control.Concurrent.Forkable (ForkableMonad (forkIO), ThreadId, threadDelay)
import Control.Monad (forever)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader
import Data.Time (NominalDiffTime, getCurrentTime)
import Database.Esqueleto
import UnliftIO (UnliftIO (unliftIO))

minuteMcS = 6000000

allProcessedDecisions ::
  ( MonadIO m,
    BackendCompatible SqlBackend backend,
    PersistQueryRead backend,
    PersistUniqueRead backend
  ) =>
  ReaderT backend m [Entity BM.Decision]
allProcessedDecisions = do
  select $
    distinct $ from \decision -> do
      where_ $ (decision ^. BM.DecisionStatus) ==. val Process
      pure decision

makeDecisions :: (MonadIO m, MonadReader Config m, ForkableMonad m) => m ()
makeDecisions = do
  v <- BM.runDb allProcessedDecisions
  pure ()

startSheduler :: (MonadIO m, MonadReader Config m, ForkableMonad m) => m ()
startSheduler = void $
  forkIO . forever $ do
    makeDecisions
    liftIO $ threadDelay minuteMcS
    pure ()
