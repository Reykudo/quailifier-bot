{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Config where

-- import Control.Monad.Metrics (Metrics, MonadMetrics, getMetrics)

import Control.Concurrent (ThreadId)
import Control.Exception.Safe (MonadCatch, throwIO)
-- import Control.Monad.Metrics (Metrics, MonadMetrics, getMetrics)

import Control.Monad (liftM)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class
import Control.Monad.Logger (LogLevel (LevelDebug, LevelError, LevelInfo, LevelOther, LevelWarn), MonadLogger (..), MonadLoggerIO)
import qualified Control.Monad.Logger as FastLogger
import Control.Monad.Logger.CallStack (MonadLoggerIO (askLoggerIO))
import Control.Monad.Reader (MonadIO, MonadReader (ask), ReaderT, asks)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.Persist.Postgresql
  ( ConnectionPool,
    ConnectionString,
    SqlBackend,
    createPostgresqlPool,
    liftSqlPersistMPool,
  )
import qualified Katip
import Logger
import qualified Network.HTTP.Simple as HS
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Servant.Client (ClientError)
import Servant.Server.Internal.ServerError
import System.Environment (lookupEnv)
import qualified TgBotAPI as TG
import TgBotAPI.Common (Configuration (Configuration, configBaseURL, configSecurityScheme), MonadHTTP (httpBS), anonymousSecurityScheme)
import UnliftIO (MonadUnliftIO, UnliftIO (unliftIO))

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype AppT m a = AppT
  { runAppT :: ReaderT Config (ExceptT ClientError m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Config,
      MonadError ClientError,
      MonadIO
    )

type App = AppT IO

instance MonadHTTP App where
  httpBS = HS.httpBS

-- | The Config for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Config = Config
  { configPool :: ConnectionPool,
    configEnv :: Environment,
    -- , configMetrics   :: Metrics
    configEkgServer :: ThreadId,
    configLogEnv :: LogEnv,
    configPort :: Port,
    configToken :: T.Text,
    configTgMaxHandlers :: Int64
  }

runMethod :: (MonadReader Config m, MonadIO m) => (Configuration -> a -> IO b) -> a -> m b
runMethod method val = do
  cfg <- ask
  let token = configToken cfg
  liftIO $ method Configuration {configBaseURL = "https://api.telegram.org/bot" <> token <> "", configSecurityScheme = anonymousSecurityScheme} val

getMethodConfiguretion :: (MonadReader Config m, MonadIO m) => m Configuration
getMethodConfiguretion = do
  cfg <- ask
  let token = configToken cfg
  pure $ Configuration {configBaseURL = "https://api.telegram.org/bot" <> token <> "", configSecurityScheme = anonymousSecurityScheme}

-- instance (MonadIO m, Katip IO) => MonadLoggerIO (KatipT m) where
--   askLoggerIO = pure $ adapt logMsg

-- instance Monad m => MonadMetrics (AppT m) where
--     getMetrics = asks Config.configMetrics

-- | Katip instance for @AppT m@
instance MonadIO m => Katip (AppT m) where
  getLogEnv = asks configLogEnv
  localLogEnv = error "not implemented"

-- instance (MonadReader Config m, MonadIO m) => Katip m where
--   getLogEnv = asks configLogEnv
--   localLogEnv = error "not implemented"

-- | MonadLogger instance to use within @AppT m@
instance MonadIO m => MonadLogger (AppT m) where
  monadLoggerLog = adapt logMsg

-- | MonadLogger instance to use in @makePool@
instance MonadIO m => MonadLogger (KatipT m) where
  monadLoggerLog = adapt logMsg

instance (MonadIO m, Katip m, MonadLogger m) => MonadLoggerIO m where
  askLoggerIO = do
    logEnv <- getLogEnv
    pure (\a b c d -> runKatipT logEnv $ monadLoggerLog a b c d)

-- pure a

-- not sure how fast this is going to be

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

-- | Web request logger (currently unimplemented and unused). For inspiration
-- see ApacheLogger from wai-logger package.
katipLogger :: LogEnv -> Middleware
katipLogger env app req respond = runKatipT env $ do
  -- todo: log proper request data
  logMsg "web" InfoS "todo: received some request"
  liftIO $ app req respond

-- | This function creates a 'ConnectionPool' for the given environment.
-- For 'Development' and 'Test' environments, we use a stock and highly
-- insecure connection string. The 'Production' environment acquires the
-- information from environment variables that are set by the keter
-- deployment application.
-- makePool :: Environment -> LogEnv -> IO ConnectionPool
makePool :: Environment -> KatipT IO ConnectionPool
makePool Test = createPostgresqlPool (connStr "-test") (envPool Test)
makePool Development = createPostgresqlPool (connStr "") (envPool Development)
makePool Production = do
  -- This function makes heavy use of the 'MaybeT' monad transformer, which
  -- might be confusing if you're not familiar with it. It allows us to
  -- combine the effects from 'IO' and the effect of 'Maybe' into a single
  -- "big effect", so that when we bind out of @MaybeT IO a@, we get an
  -- @a@. If we just had @IO (Maybe a)@, then binding out of the IO would
  -- give us a @Maybe a@, which would make the code quite a bit more
  -- verbose.
  pool <-
    runMaybeT $ do
      let keys =
            [ "host=",
              "port=",
              "user=",
              "password=",
              "dbname="
            ]
          envs =
            [ "PGHOST",
              "PGPORT",
              "PGUSER",
              "PGPASS",
              "PGDATABASE"
            ]
      envVars <- traverse (MaybeT . liftIO . lookupEnv) envs
      let prodStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
      pure $ createPostgresqlPool prodStr (envPool Production)
  case pool of
    -- If we don't have a correct database configuration, we can't
    -- handle that in the program, so we throw an IO exception. This is
    -- one example where using an exception is preferable to 'Maybe' or
    -- 'Either'.
    Nothing -> liftIO $ throwIO (userError "Database Configuration not present in environment.")
    Just a -> a

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

-- | A basic 'ConnectionString' for local/test development. Pass in either
-- @""@ for 'Development' or @"test"@ for 'Test'.
connStr :: BS.ByteString -> ConnectionString
connStr sfx = "host=localhost dbname=mydb" <> sfx <> " user=postgres password=452565 port=5432"
