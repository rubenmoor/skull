{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HttpApp.BotKey.Api.Types where

import           Data.Aeson           (FromJSON, ToJSON)
import           GHC.Generics         (Generic)

import           Servant.Docs         (ToSample (..), noSamples)

import           HttpApp.BotKey.Types (BotKey)

data BotKeyNewRequest = BotKeyNewRequest
  {
  } deriving (Generic, FromJSON, ToJSON)

instance ToSample BotKeyNewRequest where
  -- todo: proper samples
  toSamples _ = noSamples

data BotKeyNewResponse = BotKeyNewResponse
  deriving (Generic, ToJSON)

instance ToSample BotKeyNewResponse where
  toSamples _ = noSamples

data BotKeyAllResponse = BotKeyAllResponse
  { barBotKeys :: [BotKey]
  } deriving (Generic, ToJSON)

instance ToSample BotKeyAllResponse where
  toSamples _ = noSamples
