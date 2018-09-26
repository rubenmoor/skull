{-# LANGUAGE TemplateHaskell #-}

module Game.Play.Types where

import           Control.Lens            (makeLenses)
import           Control.Monad.Except    (ExceptT)
import           Control.Monad.Random    (Rand, StdGen)
import           Control.Monad.Trans.RWS (RWST)

import           Game.Api.Types          (GameError)
import           Game.Types              (Game, Player)

type WithGame a = RWST Seating () Game (ExceptT GameError (Rand StdGen)) a
type WithPlayer a = ExceptT GameError (Rand StdGen) a

data Seating = Seating
  { _seatMe    :: Player
  , _seatLeft  :: [Player] -- players sitting to the left
  , _seatRight :: [Player] -- players sitting to the right
  }

makeLenses ''Seating
