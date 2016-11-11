{-# LANGUAGE OverloadedStrings #-}

module Discord.Token
    ( -- * The @Token@ type class
      Token(..)
      -- ** The @BotToken@ data type
    , BotToken(..)
      -- ** The @BearerToken@ data type
    , BearerToken(..)
    ) where

import qualified Data.Text as T

-- | This type class represents all the way we can authenticate
-- with the Discord API.
class Token a where
  toHeader :: a -> (T.Text, T.Text)
  isBot :: a -> Bool

-- | For now Discord only provided two kind of authentication:
-- User or Bot tokens.
data BotToken = BotToken String deriving (Show, Eq)
data BearerToken = BearerToken String deriving (Show, Eq)

-- Token type class instances for these data types.

instance Token BotToken where
  toHeader (BotToken s) = ("Authorization", T.pack $ "Bot " ++ s)
  isBot _ = True

instance Token BearerToken where
  toHeader (BearerToken s) = ("Authorization", T.pack $ "Bearer " ++ s)
  isBot _ = False
