{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE Rank2Types     #-}
-- |

module Types where

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (Connection)
import           GHC.Generics               (Generic)

-- technical types

data Env = Env
  { envDbConnection :: Connection
  }

data AppError
  = ErrUser Text
  | ErrBug  Text
  | ErrDatabase Text

-- game types

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
