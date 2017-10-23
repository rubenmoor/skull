{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HttpApp.BotKey.Api.Types where

import           Data.Aeson           (FromJSON, ToJSON)
import           GHC.Generics         (Generic)

import           HttpApp.BotKey.Types (BotKey, Label, Secret)

data BKNewResp = BKNewResp
  { _nrespBotKey  :: BotKey
  } deriving (Generic, ToJSON)

data BKAllResp = BKAllResp
  { _arespBotKeys :: [BotKey]
  } deriving (Generic, ToJSON)

data BKSetLabelRq = BKSetLabelRq
  { _slrqSecret :: Secret
  , _slrqLabel  :: Label
  } deriving (Generic, FromJSON)

data BKSetLabelResp = BKSetLabelResp
  { _slrespLabel :: Label
  } deriving (Generic, ToJSON)

data BKDeleteRq = BKDeleteRq
  { _drqSecret :: Secret
  } deriving (Generic, FromJSON)
