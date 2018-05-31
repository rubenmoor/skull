{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HttpApp.PlayNow.Api.Types where

import           Prelude      (Int)

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Maybe   (Maybe (..))
import           GHC.Generics (Generic)

import           Game.Types   (Game, GameKey, PlayerKey)

newtype PNNewRq = PNNewRq
  { _nrqNumPlayers :: Int
  } deriving (Generic, FromJSON)

data PNNewResp = PNNewResp
  { _nrespGame      :: Game
  , _nrespPlayerKey :: PlayerKey
  } deriving (Generic, ToJSON)

newtype PNActiveResp = PNActiveResp (Maybe PNActive)
  deriving (Generic, ToJSON)

data PNActive = PNActive
  { _activeGame      :: Game
  , _activePlayerKey :: PlayerKey
  } deriving (Generic, ToJSON)

newtype PNDeleteRq = PNDeleteRq
  { _drqKey :: GameKey
  } deriving (Generic, FromJSON)
