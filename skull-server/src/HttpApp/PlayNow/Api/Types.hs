{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HttpApp.PlayNow.Api.Types where

import           Prelude      (Int)

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Maybe   (Maybe (..))
import           GHC.Generics (Generic)

import           Game.Types   (Game, GameKey)

newtype PNNewRq = PNNewRq
  { _nrqNumPlayers :: Int
  } deriving (Generic, FromJSON)

newtype PNNewResp = PNNewResp
  { _nrespGame :: Game
  } deriving (Generic, ToJSON)

newtype PNAllResp = PNAllResp
  { _arespGame :: Maybe Game
  } deriving (Generic, ToJSON)

newtype PNDeleteRq = PNDeleteRq
  { _drqKey :: GameKey
  } deriving (Generic, FromJSON)
