{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Discord
import Discord.Token
import Data.Aeson (Value(..))

main :: IO ()
main = do
  cfg <- defaultDiscordConfig (BearerToken "token")
  i <- runDiscordLogging cfg $ do
    o :: Either DiscordError Value <- query "lol" "lol" []
    p :: Either DiscordError Value <- query "lol" "lol" []
    liftIO $ print o
    liftIO $ print p
    return ("test" :: String)
  putStrLn i
