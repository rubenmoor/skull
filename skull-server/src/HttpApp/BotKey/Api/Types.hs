{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HttpApp.BotKey.Api.Types where

import           Data.Aeson           (FromJSON, ToJSON)
import           GHC.Generics         (Generic)

import           HttpApp.BotKey.Types (BotKey, Label, Secret)

data BotKeyNewResponse = BotKeyNewResponse
  { bnrBotKey :: BotKey
  } deriving (Generic, ToJSON)

data BotKeyAllResponse = BotKeyAllResponse
  { barBotKeys :: [BotKey]
  } deriving (Generic, ToJSON)

data BotKeySetLabelRequest = BotKeySetLabelRequest
  { bsrSecret :: Secret
  , bsrLabel  :: Label
  } deriving (Generic, FromJSON)

data BotKeySetLabelResponse = BotKeySetLabelResponse
  { bsresLabel :: Label
  } deriving (Generic, ToJSON)
