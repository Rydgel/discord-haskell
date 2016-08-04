{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Discord
    ( DiscordConfig(..)
    ) where


import Control.Exception.Base (Exception)
import Control.Exception.Lifted (throwIO, catch)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (logDebug, LoggingT, runStderrLoggingT, MonadLogger, NoLoggingT(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (runResourceT)
import Network.HTTP.Client (parseUrl, newManager, httpLbs, RequestBody(..), Response(..), HttpException(..), Request(..), Manager)
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import qualified Data.Text as T
import Control.Applicative ((<$>))


data DiscordConfig = DiscordConfig
  { dcApiKey  :: DiscordApiKey
  , dcManager :: Manager
  }

-- | Convenience method to ask ReaderT for the current API key.
askApiKey :: DiscordT m DiscordApiKey
askApiKey = dcApiKey <$> ask

-- | Creates a DiscordConfig with a new Manager.
defaultDiscordConfig :: MonadIO m => DiscordApiKey -> m DiscordConfig
defaultDiscordConfig apiKey = do
  man <- liftIO $ newManager tlsManagerSettings
  return $ DiscordConfig apiKey man

-- | The Discord monad, which supports exception and logging
type DiscordT m a = (MonadIO m, MonadLogger m, MonadBaseControl IO m) => ReaderT DiscordConfig m a

-- | Runs Discord in a monad transformer.
runDiscordT :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => DiscordConfig -> DiscordT m a -> m a
runDiscordT config action =
  runReaderT action config

-- | Runs Discord in IO, ignoring the existing monadic context and without logging.
runDiscord :: (MonadIO m) => (DiscordConfig -> DiscordT (NoLoggingT IO) a -> m a)
runDiscord config action =
  liftIO $ runNoLoggingT $ runReaderT action config

-- | Runs Discord in IO, ignoring the existing monadic context and logging to stderr.
runDiscordLogging :: (MonadIO m) => (DiscordConfig -> DiscordT (LoggingT IO) a -> m a)
runDiscordLogging config action =
  liftIO $ runStderrLoggingT $ runReaderT action config

-- | Represents a Discord API key, which implicitly includes a type (bot/bearer).
data DiscordApiKey = DiscordApiKey
  { dakApiKey :: T.Text -- Full API key including
  , dakType   :: T.Text -- Bot or Bearer
  }
  deriving (Show, Eq)

-- | Create a DiscordApiKey from Text
-- | todo response parsing
discordKey :: T.Text -> T.Text -> DiscordApiKey
discordKey apiKey apiType =
  DiscordApiKey
    { dakApiKey = apiKey
    , dakType = apiType
    }
