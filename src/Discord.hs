{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Discord
    ( runDiscordT
    , runDiscord
    , runDiscordLogging
    , DiscordConfig(..)
    , askApiKey
    , defaultDiscordConfig
    , DiscordApiKey(..)
    , DiscordError(..)
    , query
    , query'
    ) where


import Control.Exception (SomeException)
import Control.Exception.Base (Exception)
import Control.Exception.Lifted (throwIO, catch, try)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (logDebug, LoggingT, runStderrLoggingT, MonadLogger, NoLoggingT(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (runResourceT)
import Network.HTTP.Client (parseUrlThrow, newManager, httpLbs, RequestBody(..), Response(..), HttpException(..), Request(..), Manager)
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import qualified Data.Text as T
import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Control.Monad (mzero, MonadPlus, when)
import Data.Typeable (Typeable)
import Data.Aeson (FromJSON(..), (.:), Value(..), decode, (.=), object)
import Data.Aeson.Types (Pair)
import Data.Aeson.Encode (encode)
import Data.ByteString.Lazy (fromStrict)


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

headerDiscordKey :: DiscordApiKey -> T.Text
headerDiscordKey (DiscordApiKey apiKey apiType) =
  apiType <> " " <> apiKey

userAgent :: T.Text
userAgent = "DiscordBot (https://github.com/Rydgel/discord-haskell, v0.1.0.0)"


-- | Perform a query to Discord. Should only be necessary to call directly for
--   unimplemented methods. If you infer a return type of Value you will get the
--   raw JSON object or array. Automatically adds apikey to the request.
query :: (FromJSON a) => T.Text
      -- ^ Discord API section
      -> T.Text
      -- ^ Discord API method
      -> [Pair]
      -- ^ Request info
      -> DiscordT m (Either DiscordError a)
query section apiMethod request = do
  apikey <- dakApiKey <$> askApiKey
  try $ query' section apiMethod $ filterObject ("apikey" .= apikey : request)

-- | Perform a query to Discord. Should only be necessary to call directly for
--   unimplemented methods.
query' :: (FromJSON a) => T.Text -> T.Text -> Value -> DiscordT m a
query' section apiMethod request = do
  config <- ask
  initReq <- liftIO $ parseUrlThrow "https://api.discordapp.com/api/channels/1"
  let req = initReq { requestBody = RequestBodyLBS $ encode request
                    , method = methodPost
                    }
  $(logDebug) $ T.pack . show $ req
  response <- catch (liftIO (httpLbs req $ dcManager config)) catchHttpException
  $(logDebug) $ T.pack . show $ responseBody response
  case decode $ responseBody response of
    Just result -> return result
    Nothing -> throwIO $ OtherDiscordError (-1) "Could not parse result JSON from Discord"

-- | todo Add all cases possible if you want
catchHttpException :: HttpException -> DiscordT m a
catchHttpException e@(StatusCodeException _ headers _) = do
  $(logDebug) $ T.pack . show $ lookup "X-Response-Body-Start" headers
  let cantParseError = OtherDiscordError (-1) "Could not parse result JSON from Discord"
  maybe (throwIO cantParseError) throwIO (decodeError headers)
catchHttpException e = throwIO $ BadRequest 0 "Bad Request"

decodeError :: ResponseHeaders -> Maybe DiscordError
decodeError headers = fromStrict <$> lookup "X-Response-Body-Start" headers >>= decode

data DiscordError = BadRequest Int T.Text
                  | OtherDiscordError Int T.Text
  deriving (Typeable, Show, Eq)

instance Exception DiscordError

instance FromJSON DiscordError where
  parseJSON (Object v) = do
    code <- v .: "code"
    message <- v .: "message"
    return $ errConstructor code message
   where
      errConstructor code message = case (code :: Int) of
        0 -> BadRequest code message
        _ -> OtherDiscordError code message
  parseJSON _ = mzero



-- move that
-- | Creates Aeson objects after filtering to remove null values.
filterObject :: [Pair] -> Value
filterObject list =
  object $ filter notNothing list
 where
  notNothing (_, Null) = False
  notNothing _ = True
