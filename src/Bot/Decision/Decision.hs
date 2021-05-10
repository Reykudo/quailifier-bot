module Bot.Decision.Decision where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.Cont (MonadIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Time (NominalDiffTime, getCurrentTime)

type JobId = ThreadId

type Job = IO

minuteMcS = 60000000

action :: MonadIO m => m ()
action = do
  pure ()

startSheduler :: MonadIO m => m ThreadId
startSheduler = (liftIO . forkIO . forever) $ do
  threadDelay minuteMcS
  action
