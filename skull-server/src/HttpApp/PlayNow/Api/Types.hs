{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HttpApp.PlayNow.Api.Types where

import           Prelude      (Int)

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Maybe   (Maybe (..))
import           GHC.Generics (Generic)

import           Game.Types   (GameKey, Info)

newtype PNNewRq = PNNewRq
  { _nrqNumPlayers :: Int
  } deriving (Generic, FromJSON)

newtype PNNewResp = PNNewResp
  { _nrespInfo :: Info
  } deriving (Generic, ToJSON)

newtype PNAllResp = PNAllResp
  { _arespInfo :: Maybe Info
  } deriving (Generic, ToJSON)

newtype PNDeleteRq = PNDeleteRq
  { _drqKey :: GameKey
  } deriving (Generic, FromJSON)
