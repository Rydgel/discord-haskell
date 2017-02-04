{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Discord
    ( runDiscordT
    , runDiscord
    , runDiscordLogging
    , DiscordConfig (..)
    , askApiKey
    , defaultDiscordConfig
    , DiscordError (..)
    , query
    , query'
      -- * Re-exports
    , module Discord.Types
    ) where


import           Control.Exception.Base      (Exception)
import           Control.Exception.Lifted    (catch, throwIO, try)
import           Control.Monad               (mzero)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (LoggingT, MonadLogger,
                                              NoLoggingT (..), logDebugN,
                                              runStderrLoggingT)
import           Control.Monad.Reader        (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson                  (FromJSON (..), Value (..), decode,
                                              object, (.:), (.=))
import           Data.Aeson.Encode           (encode)
import           Data.Aeson.Types            (Pair)
import qualified Data.ByteString.Char8       as BS
import           Data.ByteString.Lazy        (fromStrict)
import qualified Data.CaseInsensitive        as CI
import qualified Data.Text                   as T
import           Data.Typeable               (Typeable)
import           Discord.Types
import           Network.HTTP.Client         (HttpException (..),
                                              HttpExceptionContent (..), brRead,
                                              responseHeaders, responseStatus)
import           Network.HTTP.Req
import           Network.HTTP.Types          (ResponseHeaders)

instance MonadHttp IO where
  handleHttpException = throwIO

-- | The Discord monad, which supports exception and logging
type DiscordT t m a = (MonadIO m, MonadLogger m, MonadBaseControl IO m) => ReaderT (DiscordConfig t) m a

-- | Runs Discord in a monad transformer.
runDiscordT :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => DiscordConfig t -> DiscordT t m a -> m a
runDiscordT config action =
  runReaderT action config

-- | Runs Discord in IO, ignoring the existing monadic context and without logging.
runDiscord :: MonadIO m => (DiscordConfig t -> DiscordT t (NoLoggingT IO) a -> m a)
runDiscord config action =
  liftIO $ runNoLoggingT $ runReaderT action config

-- | Runs Discord in IO, ignoring the existing monadic context and logging to stderr.
runDiscordLogging :: MonadIO m => (DiscordConfig t -> DiscordT t (LoggingT IO) a -> m a)
runDiscordLogging config action =
  liftIO $ runStderrLoggingT $ runReaderT action config

newtype DiscordConfig a = DiscordConfig { dcApiKey :: a }

-- | Convenience method to ask ReaderT for the current API key.
askApiKey :: MonadIO m => ReaderT (DiscordConfig t) m t
askApiKey = dcApiKey <$> ask

-- | Creates a DiscordConfig with a new Manager.
defaultDiscordConfig :: MonadIO m => t -> m (DiscordConfig t)
defaultDiscordConfig apiKey = return $ DiscordConfig apiKey

userAgent :: T.Text
userAgent = "DiscordBot (https://github.com/Rydgel/discord-haskell, v0.1.0.0)"

-- | Perform a query to Discord. Should only be necessary to call directly for
--   unimplemented methods. If you infer a return type of Value you will get the
--   raw JSON object or array. Automatically adds apikey to the request.
query :: (FromJSON a, Token t)
      => T.Text
      -- ^ Discord API section
      -> T.Text
      -- ^ Discord API method
      -> [Pair]
      -- ^ Request info
      -> DiscordT t m (Either DiscordError a)
query section apiMethod request = do
  apikey <- askApiKey
  try $ query' section apiMethod $ filterObject ("apikey" .= toHeader apikey : request)

-- | Perform a query to Discord. Should only be necessary to call directly for
--   unimplemented methods.
query' :: FromJSON a => T.Text -> T.Text -> Value -> DiscordT t m a
query' section apiMethod request = do
  config <- ask
  let url = https "discordapp.com" /: "api" /: "channels" /: "1"
  r <- catch (liftIO (req POST url NoReqBody lbsResponse mempty)) catchHttpException
  logDebugN $ T.pack . show $ responseBody r
  case decode $ responseBody r of
    Just result -> return result
    Nothing -> throwIO $ OtherDiscordError (-1) "Could not parse result JSON from Discord"

catchHttpException :: Network.HTTP.Req.HttpException -> DiscordT t m a
catchHttpException (VanillaHttpException (HttpExceptionRequest _ (StatusCodeException response body))) = do
  logDebugN $ T.pack . show $ response
  logDebugN $ T.pack . show $ body
  let status = responseStatus response
  logDebugN $ T.pack . show $ status
  let cantParseError = OtherDiscordError (-1) "Could not parse result JSON from Discord"
  maybe (throwIO cantParseError) throwIO (decodeError body)
catchHttpException e = throwIO $ BadRequest 0 "Bad Request"

decodeError :: BS.ByteString -> Maybe DiscordError
decodeError body = decode (fromStrict body)

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
