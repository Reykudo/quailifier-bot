{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Init where

-- import Api (app)
-- import Api.User (generateJavaScript)

import qualified Bot.DbModels as DB
-- import qualified Control.Monad.Metrics as M

-- import Network.Wai.Metrics (metrics, registerWaiMetrics)

import Bot.Decision.Decision (startSheduler)
import Bot.UpdateHandler (updateHandler)
import Bot.UpdateLoop (updateLoop)
import Config (App, AppT (runAppT), Config (..), Environment (..), makePool, setLogger)
import Control.Concurrent (killThread)
import Control.Exception.Safe
import Control.Monad.Except (MonadTrans (lift), runExceptT)
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Monoid
import qualified Data.Pool as Pool
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Typeable
import Database.Persist.Postgresql (runSqlPool)
import Katip (LogEnv (LogEnv))
import qualified Katip
import Lens.Micro ((^.))
import Logger (defaultLogEnv)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Safe (readMay)
import Say
import System.Environment (lookupEnv)
import System.Remote.Monitoring (forkServer, serverMetricStore, serverThreadId)
import UnliftIO (MonadIO (liftIO), MonadUnliftIO, UnliftIO (unliftIO))
import Web.Telegram.API (Token (..))

-- | An action that creates a WAI 'Application' together with its resources,
--   runs it, and tears it down on exit
runAppDevel :: IO ()
runAppDevel = do
  say "in runAppDevel"
  withConfig $ \config -> do
    say "acquired config"
    initialize config `finally` say "exited: initialize config"

-- say "post-initialize"
-- run (configPort config) cfg
--   `finally` say "server is closed"

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize cfg = do
  do
    say "initialize"
    -- waiMetrics <- registerWaiMetrics (configMetrics cfg ^. M.metricsStore)
    say "wai metrics"
    let logger = setLogger (configEnv cfg)
    say "run migrations"
    liftIO $
      bracket
        (say "starting to run migrations")
        (const $ say "migrations complete")
        ( const $ do
            say "actually running migrations"
            runSqlPool DB.doMigrations (configPool cfg) `catch` \(SomeException e) -> do
              say $
                mconcat
                  [ "exception in doMigrations, type: ",
                    tshow (typeOf e),
                    ", shown: ",
                    tshow e
                  ]
              throwIO e
            say "okay all done"
            -- pure $ pure ()
        )
    say "making app"
    let withRunInIO = \runInIO -> do
          say "start sheduler"
          runInIO $ startSheduler
          updateLoop cfg (runInIO . updateHandler)
    withRunInIO
      ( \appT -> do
          runExceptT $ runReaderT (runAppT appT) cfg
          pure ()
      )

-- withConfig :: (Config -> IO b) -> App ()
withConfig :: (Config -> IO ()) -> IO ()
withConfig action = do
  say "acquireConfig"
  port <- lookupSetting "PORT" 8081
  say $ "on port:" <> tshow port
  env <- lookupSetting "ENV" Development
  token <- lookupSetting "TG_BOT_QUALIFIER_TOKEN" ""
  maxHandlers <- lookupSetting "MAX_HANDLERS" 240
  say $ "on env: " <> tshow env

  bracket defaultLogEnv (\x -> say "closing katip scribes" >> Katip.closeScribes x) \logEnv -> do
    say "got log env"
    !pool <- Katip.runKatipT logEnv (makePool env) `onException` say "exception in makePool"
    say "got pool "
    liftIO $
      bracket (forkServer "localhost" 8082) (\x -> say "closing ekg" >> do killThread $ serverThreadId x) $ \ekgServer -> do
        say "forked ekg server"
        action
          Config
            { configPool = pool,
              configEnv = env,
              -- , configMetrics = metr
              configLogEnv = logEnv,
              configPort = port,
              configEkgServer = serverThreadId ekgServer,
              configToken = Token $ T.pack token,
              configTgMaxHandlers = maxHandlers
            }

shutdownApp :: Config -> IO ()
shutdownApp cfg = do
  Katip.closeScribes (configLogEnv cfg)
  Pool.destroyAllResources (configPool cfg)
  -- Monad.Metrics does not provide a function to destroy metrics store
  -- so, it'll hopefully get torn down when async exception gets thrown
  -- at metrics server process
  killThread (configEkgServer cfg)
  pure ()

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing ->
      return def
    Just str ->
      maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
      error $
        mconcat
          [ "Failed to read [[",
            str,
            "]] for environment variable ",
            env
          ]

tshow :: Show a => a -> Text
tshow = Text.pack . show
