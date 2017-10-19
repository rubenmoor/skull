{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HttpApp.BotKey.Api.Types where

import           Data.Aeson           (FromJSON, ToJSON)
import           GHC.Generics         (Generic)

import           HttpApp.BotKey.Types (BotKey, Label, Secret)

data BotKeyNewResponse = BotKeyNewResponse
  { _bnrBotKey :: BotKey
  } deriving (Generic, ToJSON)

data BotKeyAllResponse = BotKeyAllResponse
  { _barBotKeys :: [BotKey]
  } deriving (Generic, ToJSON)

data BotKeySetLabelRequest = BotKeySetLabelRequest
  { _bsrSecret :: Secret
  , _bsrLabel  :: Label
  } deriving (Generic, FromJSON)

data BotKeySetLabelResponse = BotKeySetLabelResponse
  { _bsresLabel :: Label
  } deriving (Generic, ToJSON)

data BotKeyDeleteRequest = BotKeyDeleteRequest
  { _bdrSecret :: Secret
  } deriving (Generic, FromJSON)
