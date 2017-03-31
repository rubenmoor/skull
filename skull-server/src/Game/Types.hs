{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Game.Types where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

type GameId = Text
type BotId = Text
type PlayerId = Text

type Round = Int

data Phase
  = FirstCard
  | CardOrBet
  | Bet
  | Reveal
  deriving (Generic, ToJSON)

data Card
  = Skull
  | Plain
  deriving (Generic, ToJSON, FromJSON)

data Hand = Hand
  { handNumPlains :: Int
  , handHasSkull  :: Bool
  } deriving (Generic, ToJSON)

data Stacks = Stacks [Int]
  deriving (Generic, ToJSON)

data MyStack = MyStack [Card]
  deriving (Generic, ToJSON)

data Bets = Bets [Int]
  deriving (Generic, ToJSON)
